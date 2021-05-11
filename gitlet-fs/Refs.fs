module Refs

open System.Text.RegularExpressions

let isRef (ref: string) =
    let heads = [ "HEAD"; "FETCH_HEAD"; "MERGE_HEAD" ]

    Util.matches "^refs/heads/[A-Za-z-]+$" ref
    || Util.matches "^refs/remotes/[A-Za-z-]+/[A-Za-z-]+$" ref
    || List.contains ref heads

let isHeadDetached =
    not (
        Files.read (Files.gitletPath "HEAD")
        |> Util.matches "refs"
    )

// **toLocalRef()** converts the branch name `name` into a qualified
// local branch ref.
let toLocalRef name = "refs/heads/" + name

// terminalRef resolves `ref` to the most specific ref possible.
let terminalRef ref =
    // If `ref` is "HEAD" and head is pointing at a branch, return the branch.
    if ref = "HEAD" && not isHeadDetached then
        let rx = Regex("ref: (refs/heads/.+)")

        let m =
            rx.Match <| Files.read (Files.gitletPath ("HEAD"))

        if m.Success then
            m.Groups.[1].Value
        else
            failwith "Invalid HEAD reference"
    else if isRef ref then
        // If ref is qualified, return it.
        ref
    else
        // Otherwise, assume ref is an unqualified local ref (like
        // `master`) and turn it into a qualified ref (like
        // `refs/heads/master`)
        toLocalRef ref

// **fetchHeadBranchToMerge()** reads the `FETCH_HEAD` file and gets
// the hash that the remote `branchName` is pointing at.  For more
// information about `FETCH_HEAD` see [gitlet.fetch()](#section-80).
let fetchHeadBranchToMerge branchName =
    Util.lines (Files.read (Files.gitletPath ("FETCH_HEAD")))
    |> Array.filter (fun line -> Util.matches ("^.+ branch " + branchName + " of") line)
    |> Array.map
        (fun line ->
            let rx = Regex("^([^ ]+) ")
            rx.Match(line).Groups.[1].Value)
    |> Array.head

// **headBranchName()** returns the name of the branch that `HEAD`
// is pointing at.
let headBranchName =
    if not (isHeadDetached) then
        let rx = Regex("refs/heads/(.+)")
        let ref = Files.read (Files.gitletPath "HEAD")
        rx.Match(ref).Groups.[1].Value
    else
        failwith "head is detached"

// **exists()** returns true if the qualified ref `ref` exists.
let exists ref =
    isRef ref
    && System.IO.File.Exists(Files.gitletPath ref)

// **hash()** returns the hash that `refOrHash` points to.
let hash refOrHash =
    if Objects.exists refOrHash then
        Some refOrHash
    else
        let terminalRef = terminalRef refOrHash

        if terminalRef = "FETCH_HEAD" then
            fetchHeadBranchToMerge (headBranchName) |> Some
        else if exists terminalRef then
            Files.read (Files.gitletPath terminalRef) |> Some
        else
            failwith "illegal state in hash:" + refOrHash

let isCheckedOut branch =
    not (Config.isBare) && headBranchName = branch

let toLocalRef name = "refs/heads/" + name

let toRemoteRef remote name = "refs/remotes/" + remote + "/" + name

let write ref content =
    if isRef ref then
        Files.write (Files.gitletPath ref) content
        |> ignore

// **rm()** removes the file for the qualified ref `ref`.
let rm ref =
    if isRef ref then
        System.IO.File.Delete(Files.gitletPath ref)

// **localHeads()** returns a JS object that maps local branch names
// to the hash of the commit they point to.
let localHeads =
    System.IO.Directory.EnumerateFiles(
        System.IO.Path.Join(
            [| Files.gitletPath "."
               "refs"
               "heads" |]
        )
    )
    |> Seq.fold (fun container branch -> Map.add branch ((hash branch).Value) container) Map.empty

// **commitParentHashes()** returns the array of commits that would
// be the parents of the next commit.
let commitParentHashes =
    let headHash = hash "HEAD"

    // If the repository is in the middle of a merge, return the
    // hashes of the two commits being merged.
    if Merge.isMergeInProgress then
        [| headHash; hash "MERGE_HEAD" |]

    // If this repository has no commits, return an empty array.
    else if (headHash = None) then
        [||]

    // Otherwise, return the hash of the commit that `HEAD` is
    // currently pointing at.
    else
        [| headHash |]
