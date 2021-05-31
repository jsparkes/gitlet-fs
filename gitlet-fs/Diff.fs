module Diff

open Index
open Objects

// Diff module
// -----------

// Produces diffs between versions of the repository content.  Diffs
// are represented as JS objects that map file paths to objects that
// indicate the change required to get from the first version of the
// file (the receiver) to the second (the giver).  eg:
// <pre>{
//   file1: {
//     status: "A",
//     receiver: undefined,
//     base: undefined,
//     giver: hash(1)
//   },
//   file2: {
//     status: "C",
//     receiver: hash(b),
//     base: hash(a),
//     giver: hash(c)
//   }
// }</pre>

type DiffEntry =
    { Status: string
      Receiver: string
      Bse: string
      Giver: string }

[<Literal>]
let FILE_STATUS_ADD = "A"
[<Literal>]
let FILE_STATUS_MODIFY = "M"
[<Literal>]
let FILE_STATUS_DELETE = "D"
[<Literal>]
let FILE_STATUS_SAME = "SAME"
[<Literal>]
let FILE_STATUS_CONFLICT = "CONFLICT"

// let keys<'k, 'v when 'k : comparison> (map : Map<'k, 'v>) =
let private keys (map: Map<string, 'v>) =
    map
    |> Map.fold (fun s k _ -> Set.add k s) Set.empty

// **tocDiff()** takes three JS objects that map file paths to
// hashes of file content.  It returns a diff between `receiver` and
// `giver` (see the module description for the format).  `base` is
// the version that is the most recent commen ancestor of the
// `receiver` and `giver`.  If `base` is not passed, `receiver` is
// used as the base.  The base is only passed when getting the diff
// for a merge.  This is the only time the conflict status might be
// used.
let tocDiff (receiver: Map<string, string>) (giver: Map<string, string>) (inBase: Map<string, string>) =

    // fileStatus() takes three strings that represent different
    // versions of the content of a file.  It returns the change that
    // needs to be made to get from the `receiver` to the `giver`.
    let fileStatus receiver giver bse =
        let receiverPresent = not (Map.isEmpty receiver)
        let basePresent = not (Map.isEmpty bse)
        let giverPresent = not (Map.isEmpty giver)

        if receiverPresent
           && giverPresent
           && receiver <> giver then
            if receiver <> bse && giver <> bse then
                FILE_STATUS_CONFLICT
            else
                FILE_STATUS_MODIFY
        else if receiver = giver then
            FILE_STATUS_SAME
        else if (not receiverPresent
                 && not basePresent
                 && giverPresent)
                || (receiverPresent
                    && not basePresent
                    && not giverPresent) then
            FILE_STATUS_ADD
        else if (receiverPresent && basePresent && not giverPresent)
                || (not receiverPresent && basePresent && giverPresent) then
            FILE_STATUS_DELETE
        else
            FILE_STATUS_SAME

    // If `base` was not passed, use `receiver` as the base.
    let bse =
        if Map.isEmpty inBase then
            receiver
        else
            inBase

    // Get an array of all the paths in all the versions.
    let paths =
        Set.unionMany [| keys receiver
                         keys bse
                         keys giver |]

    paths
    |> Set.fold
        (fun container k ->
            Map.add
                k
                { Status = fileStatus receiver giver bse
                  Receiver = receiver.[k]
                  Bse = bse.[k]
                  Giver = giver.[k] }
                container)
        Map.empty<string, DiffEntry>

// **diff()** returns a diff object (see above for the format of a
// diff object).  If `hash1` is passed, it is used as the first
// version in the diff.  If it is not passed, the index is used.  If
// `hash2` is passed, it is used as the second version in the diff.
// If it is not passed, the working copy is used.
let diff hash1 hash2 =
    let a =
        if String.length hash1 = 0 then
            Index.toc
        else
            Objects.commitToc hash1

    let b =
        if String.length hash2 = 0 then
            Index.workingCopyToc
        else
            Objects.commitToc hash2

    tocDiff a b Map.empty

// **nameStatus()** takes a diff and returns a map
// from file paths to file statuses.
let nameStatus (dif: Map<string, DiffEntry>) =
    dif |> Map.filter (fun k v -> v.Status <> FILE_STATUS_SAME)
    |> Map.fold (fun container k v -> Map.add k v.Status container) Map.empty<string, string>

// **changedFilesCommitWouldOverwrite()** gets a list of files
// changed in the working copy.  It gets a list of the files that
// are different in the head commit and the commit for the passed
// hash.  It returns a list of paths that appear in both lists.
let changedFilesCommitWouldOverwrite hash =
    match Refs.hash "HEAD" with
    | Some headHash ->
        Set.intersect (keys (nameStatus (diff headHash ""))) (keys (nameStatus (diff headHash hash)))
    | None -> Set.empty

// **addedOrModifiedFiles()** returns a list of files that have been
// added to or modified in the working copy since the last commit.
let addedOrModifiedFiles =
    let hash =
        match Refs.hash "HEAD" with
        | Some str -> str
        | None -> ""

    let headToc =
        if String.length hash > 0 then
            Objects.commitToc hash
        else
            Map.empty

    nameStatus (tocDiff headToc (Index.workingCopyToc) Map.empty)
    |> Map.filter (fun _ v -> v <> FILE_STATUS_DELETE)
    |> Map.fold (fun container k v -> Set.add k container) Set.empty
