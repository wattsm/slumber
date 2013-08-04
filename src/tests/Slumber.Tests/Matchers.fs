namespace Slumber.Tests

open NHamcrest

[<AutoOpen>]
module Matchers = 

    let Some'<'a when 'a : equality> (value : 'a) = 
        {
            new NHamcrest.Matcher<obj> () with

                override this.DescribeMismatch (item, desc) = 
                    desc.AppendText (string item)
                    |> ignore

                override this.DescribeTo desc = 
                    value.ToString ()
                    |> desc.AppendText
                    |> ignore

                override this.Matches value' =
                    match value' with
                    | :? ('a option) as opt -> 
                        match opt with
                        | Some x -> (x = value)
                        | _ -> false
                    | _ -> false
        }

    let None'<'a> =
        {
            new NHamcrest.Matcher<obj> () with

                override this.DescribeMismatch (item, desc) = 
                    desc.AppendText (string item)
                    |> ignore

                override this.DescribeTo desc = 
                    "None"
                    |> desc.AppendText
                    |> ignore

                override this.Matches value' =
                    match value' with
                    | :? ('a option) as opt ->
                        match opt with
                        | None -> true
                        | _ -> false
                    | _ -> false
        }

