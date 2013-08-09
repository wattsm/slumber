namespace Slumber

open System
open Slumber.Common.Http

///Contains functions and types used to construct request pipelines
module Framework =

    ///Describes reasons for the pipeline to stop
    type StopType = 
        | Exception of Exception
        | Completed of Response

    ///Describes possible pipeline states
    type State<'TState> = 
        | Running of 'TState
        | Stopped of StopType   

    ///Gets the state resulting from the execution of a function
    let private getNextState f arg = 
        async {
            try 
                return! (f arg)
            with
            | e -> 
                return (Stopped (Exception e))
        }

    ///Binds two pipeline phases together
    let (-->) f g = 
        fun arg ->   
            async {              

                let! state = 
                    getNextState f arg

                match (state) with
                | Running arg' -> return! (g arg')
                | Stopped type' -> return (Stopped type')
            }

    ///Binds the final pipeline phase to the rest of the pipeline
    let (--|) pipeline end' = 
        fun arg ->
            async {
            
                let! state = 
                    getNextState pipeline arg

                return! (end' state)
            }

    ///Lifts an argument to the running state
    let start arg =
        async {
            return (Running arg)
        }

    ///Contains pipeline utility functions
    module Helpers =

        ///Stops execution of the pipeline with the given response
        let stopWithResponse response = 
            response
            |> Completed
            |> Stopped

        ///Stops execution of the pipline with the given status code
        let stopWithStatus statusCode = 
            {
                ResponseType = (StatusCode statusCode);
                ContentType = None;
                CustomHeaders = [];
            }
            |> Completed
            |> Stopped

        ///Stops the execution of the pipeline with an error
        let stopWithError e = 
            Exception e
            |> Stopped

        ///Continues execution of the pipeline with the given arguments
        let continue' arg = 
            Running arg

