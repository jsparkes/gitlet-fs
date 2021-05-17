module Index

// Index module
// ------------

// The index maps files to hashes of their content.  When a commit is
// created, a tree is built that mirrors the content of the index.

// Index entry keys are actually a `path,stage` combination.  Stage is
// always `0`, unless the entry is about a file that is in conflict.
// See `index.writeConflict()` for more details.

// **key()** returns an index key made from `path` and `stage`.
let key path stage = path + "," + stage

// **read()** returns the index as a Map
let read =
    let indexFilePath = Files.gitletPath "index"

    if System.IO.File.Exists(indexFilePath) then
        Files.read indexFilePath
        |> Util.lines
        |> Array.fold
            (fun container line ->
                let blobData = line.Split(' ')
                Map.add (key blobData.[0] blobData.[1]) blobData.[2] container)
            Map.empty<string, string>
    else
        Map.empty

// **hasFile()** returns true if there is an entry for `path` in the
// index `stage`.
let hasFile path stage =
    let map = read

    match Map.tryFind (key path stage) map with
    | Some str -> true
    | None -> false

type KeyPieces = { path: string; stage: string }

// **keyPieces()** returns an array that contains the path and
// stage of 'key`.
let keyPieces (key: string) =
    let pieces = key.Split(' ')

    { path = pieces.[0]
      stage = pieces.[1] }

// **toc()** returns an object that maps file paths to hashes of
// their content.  This function is like `read()`, except the JS
// object it returns only uses the file path as a key.
let toc =
    let idx = read

    Map.fold
        (fun container key value ->
            let parts = keyPieces key
            Map.add parts.path value container)
        Map.empty
        idx

// **isFileInConflict()** returns true if the file for `path` is in
// conflict.
let isFileInConflict path = hasFile path "2"

// **conflictedPaths()** returns an array of all the paths of files
// that are in conflict.
let conflictedPaths =
    let idx = read

    idx
    |> Map.filter (fun k v -> (keyPieces k).stage = "2")
    |> Map.map (fun k v -> (keyPieces k).path)

// **write()** takes a map that represents an index and writes
// it to `.gitlet/index`.
let write index =
    let entries =
        Map.fold
            (fun c k v ->
                let p = keyPieces k
                List.append c (List.singleton (p.path + " " + p.stage + " " + v)))
            []
            index

    let content = (String.concat "\n" entries) + "\n"
    Files.write (Files.gitletPath "index") content


// **writeRm()** removes the index entry for the file at `path`.
// The file will be removed from the index even if it is in
// conflict.  (See `index.writeConflict()` for more information on
// conflicts.)
let writeRm path =
    // ["0"; "1"; "2"; "3"] |> List.forEach(function(stage) { delete idx[index.key(path, stage)]; });
    read
    |> Map.filter (fun k v -> (keyPieces k).path <> path)
    |> write

// **_writeStageEntry()** adds the hashed `content` to the index at
// key `path,stage`.
let private writeStageEntry path stage content =
    read |> Map.add (key path stage) content |> write

// **writeNonConflict()** sets a non-conflicting index entry for the
// file at `path` to the hash of `content`.  (If the file was in
// conflict, it is set to be no longer in conflict.)
let writeNonConflict path content =
    // Remove all keys for the file from the index.
    writeRm path

    // Write a key for `path` at stage `0` to indicate that the
    // file is not in conflict.
    writeStageEntry path "0" content

// **writeConflict()** sets an index entry for the file
// at `path` that indicates the file is in conflict after a merge.
// `receiverContent` is the version of the file that is being merged
// into. `giverContent` is the version being merged in.
// `baseContent` is the version that the receiver and
// giver both descended from.
let writeConflict path receiverContent giverContent baseContent =
    if baseContent = "" then
        // Write a key for `path` at stage `1` for `baseContent`.
        // (There is no `baseContent` if the same file was added for the
        // first time by both versions being merged.)
        writeStageEntry path "1" baseContent |> ignore

    // Write a key for `path` at stage `2` for `receiverContent`.
    writeStageEntry path "2" receiverContent

    // Write a key for `path` at stage `3` for `giverContent`.
    writeStageEntry path "3" giverContent

// **workingCopyToc()** returns an object that maps the file paths
// in the working copy to hashes of those files' content.
let workingCopyToc =
    read
    |> Map.filter
        (fun k _ ->
            (keyPieces k).path
            |> Files.workingCopyPath
            |> System.IO.File.Exists)
    |> Map.fold
        (fun container k v ->
            let p = keyPieces k
            Map.add p.path (Files.read (Files.workingCopyPath p.path)) container)
        Map.empty

// **tocToIndex()** takes an object that maps file paths to hashes
// of the files' content.  It returns an object that is identical,
// except the keys of the object are composed of the file paths and
// stage `0`.  eg: `{ "file1,0": hash(1), "src/file2,0": hash(2) }'
let tocToIndex toc =
    toc
    |> Map.fold (fun container k v -> Map.add (key k "0") v container) Map.empty

// **matchingFiles()** returns all the paths in the index that match
// `pathSpec`.  It matches relative to `currentDir`.
let matchingFiles pathSpec =
    let searchPath = Files.pathFromRepoRoot pathSpec

    toc
    |> Map.filter
        (fun k v ->
            let p = keyPieces k
            Util.matches ("^" + searchPath) p.path)
