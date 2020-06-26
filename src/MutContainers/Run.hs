module MutContainers.Run (
    run,
    runM
) where
import Prelude

run :: () => 
    (genericInputs -> inputs) -> (inputs -> outputs) -> (outputs -> genericOutputs) ->
    (genericInputs -> genericOutputs)
run importInputs runTest exportOutputs genericInputs = exportOutputs outputs
    where
    outputs = runTest inputs
    inputs = importInputs genericInputs

runM :: (Monad m) => 
    (genericInputs -> m inputs) -> (inputs -> m outputs) -> (outputs -> m genericOutputs) ->
    (genericInputs -> m genericOutputs)
runM importInputs runTest exportOutputs genericInputs = do
    inputs <- importInputs genericInputs
    outputs <- runTest inputs
    exportOutputs outputs
