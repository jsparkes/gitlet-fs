module Status

// Outputs the repository status as a human-readable string.

// **untracked()** returns a list of lines listing the files not
// being tracked by Gitlet.
let private untracked () =
    // This should probably be recursive, right?
    System.IO.Directory.EnumerateFiles (Files.workingCopyPath ".")
    |> Seq.filter (fun p ->
        Index.toc.[p].Length > 0 && p <> ".gitlet")
    |> Seq.toList

// **toBeCommitted()** returns a list of lines listing the files
// that have changes that will be included in the next commit.
let private toBeCommitted () = 
    let toc =
        match Refs.hash("HEAD") with
        | Some hash ->
            Objects.commitToc hash
        | None ->
            Map.empty
    Diff.tocDiff toc (Index.toc) Map.empty
    |> Diff.nameStatus
    |> Map.fold (fun container k v -> List.append container (List.singleton (v + " " + k))) List.empty

// **notStagedForCommit()** returns a list of lines listing the
// files that have changes that will not be included in the next
// commit.
let private notStagedForCommit () =
    Diff.nameStatus (Diff.diff "" "")
    |> Map.fold (fun container k v -> List.append container (List.singleton (v  + " " + k))) List.empty

// **listing()** keeps `lines` (prefixed by `heading`) only if it's nonempty.
let private listing (heading: string) (lines: List<string>) =
    if not (List.isEmpty lines) then
        List.append (List.singleton heading) lines
    else
        List.empty

// **toString()** returns the repository status as a human-readable
// string.
let toString =
    // Gather all the sections, keeping only nonempty ones, and flatten them
    // together into a string.
    let lines = List.concat [["On branch " + Refs.headBranchName];
                        listing "Untracked files:" (untracked ());
                        listing "Unmerged paths:" (Util.keys (Index.conflictedPaths) |> Set.toList);
                        listing "Changes to be committed:" (toBeCommitted ());
                        listing "Changes not staged for commit:" (notStagedForCommit ())]
                |> List.toArray
    System.String.Join("\n", lines)
