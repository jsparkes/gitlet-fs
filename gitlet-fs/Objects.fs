module Objects

let objectPath hash =
    System.IO.Path.Join([| Util.gitletRoot; "objects"; hash |])

// **commitType()** parses `str` as an object and returns its type:
// commit, tree or blob.
let commitType str =
    match str with
    | "commit" -> "commit"
    | "tree" -> "tree"
    | "blob" -> "tree"
    | _ -> "blob"

// **read()** returns the content of the object called `objectHash`.
let read objectHash =
    if objectHash <> "" then
        let objectPath = objectPath objectHash

        if System.IO.File.Exists(objectPath) then
            System.IO.File.ReadAllText objectPath
        else
            failwith ("Objects.read: no file for " + objectPath)
    else
        failwith ("Objects.read: invalid hash")

// **parentHashes()** parses `str` as a commit and returns the
// hashes of its parents.
let parentHashes str =
    if commitType str = "commit" then
        Util.lines str
        |> Array.filter (fun line -> line |> Util.matches "^parent")
        |> Array.map (fun line -> line.Split(" ").[1])
    else
        Array.empty

// **ancestors()** returns an array of the hashes of all the
// ancestor commits of `commitHash`.
let rec ancestors commitHash : string[] =
    parentHashes (read commitHash)
    |> Array.collect ancestors

// **isAncestor()** returns true if `descendentHash` is a descendent
// of `ancestorHash`.
let isAncestor descendentHash ancestorHash =
    ancestors ancestorHash
    |> Array.contains descendentHash

// **isUpToDate()** returns true if the giver commit has already
// been incorporated into the receiver commit.  That is, it returns
// true if the giver commit is an ancestor of the receiver, or they
// are the same commit.
let isUpToDate receiverHash giverHash =
    receiverHash <> ""
    && (receiverHash = giverHash
        || isAncestor receiverHash giverHash)

// **exists()** returns true if there is an object in the database
// called `objectHash`
let exists objectHash =
    objectHash <> ""
    && System.IO.File.Exists(objectPath objectHash)

// **allObjects()** returns an array of the string content of all
// the objects in the database
let allObjects =
    (Seq.map
        ((fun (name: string) -> System.IO.Path.GetFileName(name))
         >> read)
        (System.IO.Directory.EnumerateFiles(System.IO.Path.Join(Util.gitletRoot, "/objects"))))
    |> Seq.toArray

// **parentHashes()** parses `str` as a commit and returns the tree
// it points at.
let treeHash (str: string) =
    if commitType str = "commit" then
        Some(str.Split(" ").[1])
    else
        None

// **fileTree()** takes a tree hash and finds the corresponding tree
// object.  It reads the connected graph of tree objects into a
// nested JS object, like:<br/>
// `{ file1: "hash(1)", src: { file2:  "hash(2)" }`
let fileTree treeHash tree =
    let contents = read treeHash

    contents
    |> Array.map
        (fun line ->
            let lineTokens = line.Split(" ")

            match lineTokens.[0] with
            | "tree" -> fileTree lineTokens.[1] (Map.empty)
            | _ -> lineTokens.[1])
//return tree;

// **commitToc()** takes the hash of a commit and reads the content
// stored in the tree on the commit.  It turns that tree into a
// table of content that maps filenames to hashes of the files'
// content, like: `{ "file1": hash(1), "a/file2": "hash(2)" }`
let commitToc hash = read hash |> treeHash |> fileTree
//return files.flattenNestedTree(objects.fileTree(objects.treeHash(objects.read(hash))));
