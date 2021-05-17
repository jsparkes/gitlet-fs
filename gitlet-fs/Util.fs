module Util

open System.Security.Cryptography
open System.Text
open System.Text.RegularExpressions

let private byteToHex (bytes: byte []) =
    bytes
    |> Array.fold (fun state x -> state + sprintf "%02X" x) ""

// Git uses SHA1 internally.
// Gitlet uses a custom hash function
let hash (str: string) =
    use sha1 = new SHA1Managed()

    str
    |> Encoding.UTF8.GetBytes
    |> sha1.ComputeHash
    |> byteToHex

// Gitlet version of hash.
let hashGitlet (str: string) =
    let mutable hashInt = 0
    // Does this handle overflow correctly compared to javascript?
    // We would like to be compatible with gitlet.js
    str
    |> Encoding.UTF8.GetBytes
    |> Array.iter (fun c -> hashInt <- hashInt * 31 + int c)

    sprintf "%x" (System.Math.Abs hashInt)

//data FileMap
//= FileMap
//  { path :: Maybe String
//  , map :: Maybe FileMap
//  }

// empty :: Unit -> FileMap
// empty = { Nothing, Nothing } :: FileMap
// Assumes array length >= 2.  No error checking in original!
// This function tries to exactly mimic the object based code
// in the original gitlet.js
// setIn :: Object String -> Maybe (Array String) -> Effect (Object String)
// setIn map array = do
//   case array of
//     Nothing -> pure $ Object.empty
//     Just arr -> case Array.length arr of
//       0 -> do
//         throw "Illegal state in setIn"
//         pure $ Object.empty
//       1 -> do
//         throw "Illegal state in setIn"
//         pure $ Object.empty
//       2 -> do
//         pure $ Object.insert (Array.unsafeIndex arr 0) (Array.unsafeIndex arr 1) map
//       _ -> do
//         obj <- setIn Object.empty (Array.tail arr)
//         pure $ map Object.insert (Array.unsafeIndex arr 0) obj
// setIn obj arr =
//     case Array.head $ Array.keys of
//     Just key ->
//     // There should only be a single key in the object.
//     maybe obj (\key ->
//                     case Array.length 2 of
//                     2 -> Some $ Object.insert key arr[1] obj
//                     ) $ Array.head $ Array.keys
//     case Array.length of
// setIn: function(obj, arr) {
//     if (arr.length === 2) {
//       obj[arr[0]] = arr[1];
//     } else if (arr.length > 2) {
//       obj[arr[0]] = obj[arr[0]] || {};
//       util.setIn(obj[arr[0]], arr.slice(1));
//     }
//     return obj;
//   },
let lines (str: string) =
    str.Split("\n", System.StringSplitOptions.RemoveEmptyEntries)

// Do we need to preserve order?
// unique :: forall t. Ord t => Array t -> Array t
// unique arr = Array.fromFoldable $ HashSet.fromFoldable arr
// unique array = Array.foldr (\val acc -> case Array.find (==) val of
//     Just x -> Array.empty
//     Nothing -> val :: acc) array
// unique: function(arr) {
//     return arr.reduce(function(a, p) { return a.indexOf(p) === -1 ? a.concat(p) : a; }, []);
//   },

// hasKey :: forall t. String -> HashMap String t -> Boolean
// hasKey key table = isJust (HashMap.lookup key table)

let matches r s =
    let rx = Regex(r)
    (rx.Match s).Success

let replace r (replacement: string) str =
    let rx = Regex(r)
    rx.Replace(str, replacement)

// **gitletRoot()** returns the absolute path of the `.gitlet` directory of the repository.
let gitletRoot =
    let rec gitletDir dir =
        if System.IO.Directory.Exists dir then
            let gitletPath =
                System.IO.Path.Join([| dir; ".gitlet" |])

            if System.IO.Directory.Exists(gitletPath) then
                Some gitletPath
            else if (dir <> "/") then
                gitletDir (System.IO.Path.Join([| dir; ".." |]))
            else
                None
        else
            None

    match gitletDir (System.IO.Directory.GetCurrentDirectory()) with
    | Some dir -> dir
    | None -> failwith "Util.gitletRoot: not in a gitlet repository"
