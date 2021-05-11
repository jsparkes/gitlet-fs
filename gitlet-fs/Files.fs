module Files

// **gitletPath()** returns a string made by concatenating `path` to
// the absolute path of the `.gitlet` directory of the repository.
let gitletPath (path: string) =
  let rec gitletDir dir =
      if System.IO.Directory.Exists dir then
          let potentialConfigFile = System.IO.Path.Join([|dir; "config"|])
          let potentialGitletPath = System.IO.Path.Join([|dir; ".gitlet"|])
          if System.IO.File.Exists(potentialConfigFile) then
              //fs.statSync(potentialConfigFile).isFile() &&
              let contents = System.IO.File.ReadAllText(potentialConfigFile)
              if Util.matches "\[core\]" contents then
                  dir
              else
                  failwith (dir + "/" + "config exists but is not a gitlet config file")
          else if System.IO.File.Exists(potentialGitletPath) then
              potentialGitletPath
          else if (dir <> "/") then
              gitletDir (System.IO.Path.Join([|dir; ".."|]))
          else
              ""
      else
          ""

  let gDir = gitletDir (System.IO.Directory.GetCurrentDirectory())
  if gDir <> "" then
    System.IO.Path.Join([|gDir; path|])
  else
    failwith "gitlet dir not found"

// **inRepo()** returns true if the current working directory is
// inside a repository.
let inRepo = gitletPath(".") <> ""

// **assertInRepo()** throws if the current working directory is not
// inside a repository.
let assertInRepo =
  not (inRepo) && failwith "not a gitlet repository"

// **workingCopyPath()** returns a string made by concatenating `path` to
// the absolute path of the root of the repository.
let workingCopyPath path =
    System.IO.Path.Join([|System.IO.Path.Join([|gitletPath("."); ".."|]); path |])

// **pathFromRepoRoot()** returns `path` relative to the repo root
let pathFromRepoRoot path =
  System.IO.Path.GetRelativePath(workingCopyPath ".", System.IO.Path.Join([|System.IO.Directory.GetCurrentDirectory(); path|]))

// **write()** writes `content` to file at `path`, overwriting
// anything that is already there.
let write path content =
    System.IO.File.WriteAllText(path, content)

// **writeFilesFromTree()** takes `tree` of files as a nested JS obj
// and writes all those files to disk taking `prefix` as the root of
// the tree.  `tree` format is: `{ a: { b: { c: "filecontent" }}}`
//writeFilesFromTree: function(tree, prefix) {
//  Object.keys(tree).forEach(function(name) {
//    var path = nodePath.join(prefix, name);
//    if (util.isString(tree[name])) {
//      fs.writeFileSync(path, tree[name]);
//    } else {
//      if (!fs.existsSync(path)) {
//        fs.mkdirSync(path, "777");
//      }

//      files.writeFilesFromTree(tree[name], path);
//    }
//  });
//},

// **rmEmptyDirs()** recursively removes all the empty directories
// inside `path`.
let rec rmEmptyDirs path =
    if System.IO.Directory.Exists(path) then
        let dirs = System.IO.Directory.EnumerateDirectories(path)
        dirs |> Seq.iter (fun dir -> rmEmptyDirs path) 
        if (Seq.isEmpty dirs) then
            System.IO.Directory.Delete(path)

// **read()** returns the contents of the file at `path` as a
// string.  It returns `undefined` if the file doesn't exist.
let read path =
    if System.IO.File.Exists(path) then
        System.IO.File.ReadAllText(path)
    else
        failwith "read: file does not exist: " + path

// **lsRecursive()** returns a list of all the files found in a
// recursive search of `path`.
let rec lsRecursive path =
    if System.IO.File.Exists path then
        [path]
    else if System.IO.Directory.Exists path then
        System.IO.Directory.EnumerateFileSystemEntries(path)
        |> Seq.toList
        |> List.fold (fun l entry -> 
                        List.append l (lsRecursive (System.IO.Path.Join([|path; entry|]))))
                     List.empty
    else
        List.empty

// **nestFlatTree()** takes `obj`, a mapping of file path strings to
// content, and returns a nested JS obj where each key represents a
// sub directory.  This is the opposite of
// `flattenNestedTree()`<br/>
// eg `nestFlatTree({ "a/b": "me" }); // => { a: { b: "me" }}`
//nestFlatTree: function(obj) {
//  return Object.keys(obj).reduce(function(tree, wholePath) {
//    return util.setIn(tree, wholePath.split(nodePath.sep).concat(obj[wholePath]));
//  }, {});
//},

// **flattenNestedTree()** takes `tree`, a nested JS object where
// each key represents a sub directory and returns a JS object
// mapping file path strings to content.  This is the opposite of
// `nestFlatTree()`<br/>
// eg `flattenNestedTree({ a: { b: "me" }}); // => { "a/b": "me"}`
//flattenNestedTree: function(tree, obj, prefix) {
//  if (obj === undefined) { return files.flattenNestedTree(tree, {}, ""); }

//  Object.keys(tree).forEach(function(dir) {
//    var path = nodePath.join(prefix, dir);
//    if (util.isString(tree[dir])) {
//      obj[path] = tree[dir];
//    } else {
//      files.flattenNestedTree(tree[dir], obj, path);
//    }
//  });

//  return obj;
//}
//};
