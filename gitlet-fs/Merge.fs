module Merge

// Merge module
// ------------

// **commonAncestor()** returns the hash of the commit that is the
// most recent common ancestor of `aHash` and `bHash`.
let commonAncestor (aHash:string) (bHash:string) =
    let sorted = Array.sort [|aHash; bHash|]
    let aHash = sorted.[0]
    let bHash = sorted.[1]
    let aAncestors = aHash : Objects.ancestors aHash
    let bAncestors = bHash : Objects.ancestors bHash
    (Array.intersection aAncestors bAncestors).[0]

// **isMergeInProgress()** returns true if the repository is in the
// middle of a merge.
let isMergeInProgress =
    match (Refs.hash "MERGE_HEAD") with
    | Some s -> true
    | None -> false
  
// **canFastForward()** A fast forward is possible if the changes
// made to get to the `giverHash` commit already incorporate the
// changes made to get to the `receiverHash` commit.  So,
// `canFastForward()` returns true if the `receiverHash` commit is
// an ancestor of the `giverHash` commit.  It also returns true if
// there is no `receiverHash` commit because this indicates the
// repository has no commits, yet.
let canFastForward receiverHash giverHash =
    String.length receiverHash = 0 || Objects.isAncestor giverHash receiverHash

// **isAForceFetch()** returns true if hash for local commit
// (`receiverHash`) is not ancestor of hash for fetched commit
// (`giverHash`).
let isAForceFetch receiverHash giverHash =
    String.length receiverHash > 0 && not (Objects.isAncestor giverHash receiverHash)

// **mergeDiff()** returns a diff that represents the changes to get
// from the `receiverHash` commit to the `giverHash` commit.
// Because this is a merge diff, the function uses the common
// ancestor of the `receiverHash` commit and `giverHash` commit to
// avoid trivial conflicts.
let mergeDiff receiverHash giverHash = 
    Diff.tocDiff (Objects.commitToc receiverHash)
                 (Objects.commitToc giverHash)
                 (Objects.commitToc (commonAncestor receiverHash giverHash))

// **hasConflicts()** returns true if merging the commit for
// `giverHash` into the commit for `receiverHash` would produce
// conflicts.
let hasConflicts receiverHash giverHash =
    mergeDiff receiverHash giverHash
    |> Map.filter (fun _ v -> v.Status = Diff.FILE_STATUS_CONFLICT)
    |> Map.count > 0

// **writeMergeMsg()** creates a message for the merge commit that
// will potentially be created when the `giverHash` commit is merged
// into the `receiverHash` commit.  It writes this message to
// `.gitlet/MERGE_MSG`.
let writeMergeMsg receiverHash giverHash ref =
    let mutable msg = "Merge " + ref + " into " + Refs.headBranchName
    let mergeDiff = mergeDiff receiverHash giverHash
    let conflicts = 
        mergeDiff
        |> Map.filter (fun _ v -> v.Status = Diff.FILE_STATUS_CONFLICT)
        |> Map.fold (fun container k v -> Set.add k container) Set.empty
        |> Set.toArray
    if Array.length conflicts > 0 then
      msg <- msg + "\nConflicts:\n" + (String.concat "\n" conflicts) + "\n"
    Files.write (Files.gitletPath "MERGE_MSG") msg

// **writeIndex()** merges the `giverHash` commit into the
// `receiverHash` commit and writes the merged content to the index.
let writeIndex receiverHash giverHash =
    let mergeDiff = mergeDiff receiverHash giverHash
    Index.write Map.empty
    mergeDiff |> Map.map (fun k v ->
        match v.Status with
        | Diff.FILE_STATUS_CONFLICT ->
            Index.writeConflict k
                (Objects.read v.Receiver)
                (Objects.read v.Giver)
                (Objects.read v.Bse)
        | Diff.FILE_STATUS_MODIFY ->
            Index.writeNonConflict k (Objects.read v.Giver)
        | Diff.FILE_STATUS_SAME
        | Diff.FILE_STATUS_ADD ->
            let r = Objects.read v.Receiver
            let content =
                if String.length r > 0 then
                    r
                else
                    Objects.read v.Giver
            Index.writeNonConflict k content
        | _ ->
            ()
    )

 // **writeFastForwardMerge()** Fast forwarding means making the
 // current branch reflect the commit that `giverHash` points at.  No
 // new commit is created.
 let writeFastForwardMerge receiverHash giverHash =
    // Point head at `giverHash`.
    Refs.write (Refs.toLocalRef (Refs.headBranchName)) giverHash

    // Make the index mirror the content of `giverHash`.
    Index.write (Index.tocToIndex (Objects.commitToc giverHash))

    // If the repo is bare, it has no working copy, so there is no
    // more work to do.  If the repo is not bare...
    if not (Config.isBare) then

      // ...Get an object that maps from file paths in the
      // `receiverHash` commit to hashes of the files' content.  If
      // `recevierHash` is undefined, the repository has no commits,
      // yet, and the mapping object is empty.
      let receiverToc = 
        if String.length receiverHash = 0 then
            Map.empty
        else
            Objects.commitToc receiverHash

      // ...and write the content of the files to the working copy.
      WorkingCopy.write (Diff.tocDiff receiverToc (objects.commitToc giverHash))

// **writeNonFastForwardMerge()** A non fast forward merge creates a
// merge commit to integrate the content of the `receiverHash`
// commit with the content of the `giverHash` commit.  This
// integration requires a merge commit because, unlike a fast
// forward merge, no commit yet exists that embodies the combination
// of these two commits.  `writeNonFastForwardMerge()` does not
// actually create the merge commit.  It just sets the wheels in
// motion.
let writeNonFastForwardMerge receiverHash giverHash giverRef =

    // Write `giverHash` to `.gitlet/MERGE_HEAD`.  This file acts as a
    // record of `giverHash` and as the signal that the repository is
    // in the merging state.
    Refs.write "MERGE_HEAD" giverHash

    // Write a standard merge commit message that will be used when
    // the merge commit is created.
    writeMergeMsg receiverHash giverHash giverRef

    // Merge the `receiverHash` commit with the `giverHash` commit and
    // write the content to the index.
    writeIndex receiverHash giverHash |> ignore

    // If the repo is bare, it has no working copy, so there is no
    // more work to do.  If the repo is not bare...
    if not (Config.isBare) then

      // ...merge the `receiverHash` commit with the `giverHash`
      // commit and write the content to the working copy.
      WorkingCopy.write (mergeDiff receiverHash giverHash)


