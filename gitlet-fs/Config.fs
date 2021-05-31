module Config

// A config file is turned into a nested Map
//   section subsection variable = value
// Map<string, Map<string, Map<string, string>>>

type ConfigType = Map<string, Map<string, Map<string, string>>>

// **strToConfig()** parses the config string `str` and returns its
// contents as a nested Map.
let strToConfig (str: string) =
    str.Split("[")
    |> Array.map (fun section -> section.Trim())
    |> Array.filter (fun section -> section <> "")
    |> Array.fold (fun cont section -> 
        let lines = section.Split("\n") |> Array.filter (fun l -> l <> "")
        let sectionHeader = lines.[0]
        let section = Util.matchGet "^(\w+)" lines.[0]
        let subsection = Util.matchGet "\"(\w+)\"" lines.[0]  // May be an empty string
        let config = 
            Array.tail lines // skip section header
            |> Array.filter (fun l -> not (Util.matches "^#" l))
            |> Array.fold (fun container line -> 
                // We don't handle syntax errors well!
                let parts = line.Split("=")
                let key = parts.[0].Trim()
                let value = parts.[1].Trim()
                Map.add key value container
                ) Map.empty
        // No Map.singleton function?
        Map.add section (Map.add subsection config Map.empty) cont) Map.empty
        // Original added a "remote" section

// **configToStr()** converts the nested map into a single str config file
let configToStr (config: ConfigType) =
    let contents = config
    |> Map.map (fun section v ->
            let vars = v |> Map.map (fun subsection v -> 
                v |> Map.map (fun k value -> sprintf "\t%s = %s\n" k value)
                if subsection = "" then
                        sprintf "[%s]\n%s" section vars
                else
                        sprintf "[%s \"%s\"]\n%s" section subsection vars
    ))
    Files.write (Files.gitletPath "config") contents


// **read()** returns the contents of the config file as a nested Map.
let read =
  Files.read (Files.gitletPath "config") |> strToObj

// **isBare()** returns true if the repository is bare.
let isBare =
  read.["core"].[""].["bare"] = "true"

// **assertNotBare()** throws if the repository is bare.
let assertNotBare =
  if isBare then
    failwith "this operation must be run in a work tree"

// **write()** stringifies the nested JS object `configObj` and
// overwrites the config file with it.
let write configObj =
  Files.write (Files.gitletPath "config") (objToStr configObj))
