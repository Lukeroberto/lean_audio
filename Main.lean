def isValidWav (path: String) : IO Bool := do
  let fp := System.FilePath.mk path
  let exist ← fp.pathExists
  let isWav := fp.extension == some "wav"
  return exist && isWav 


def loadWavToFloatArray (path : String) : IO FloatArray := do
  let bytes ← IO.FS.readBinFile path
  
  if bytes.size < 44 then
    throw (IO.userError "File too small to be a valid WAV")

  let dataSize := bytes.size - 44
  let numSamples := dataSize / 2
  -- FloatArray.empty does not take arguments
  let mut floats := FloatArray.empty 

  for i in [0 : numSamples] do
    let low := bytes.get! (44 + 2 * i)
    let high := bytes.get! (44 + 2 * i + 1)
    
    -- Lean 4 uses ||| for bitwise OR and <<< for shift
    let val := (high.toUInt16 <<< 8) ||| low.toUInt16
    
    -- Corrected conversion logic:
    -- We convert UInt16 -> Nat -> Int
    let valInt : Int := Int.ofNat val.toNat
    let signedVal : Int := if val >= 32768 then 
      valInt - 65536 
    else 
      valInt
    
    -- Convert Int to Float using the Float.ofInt helper
    floats := floats.push (Float.ofInt signedVal / 32768.0)
    
  return floats


def main (args : List String) : IO Unit := do
  match args with 
    | [path] =>
        if ← isValidWav path then
          IO.println s!"Loading {path}..."
          let signal ← loadWavToFloatArray path
          IO.println s!"Successfully loaded {signal.size} samples."
        else
          IO.println "Invalid file path or not a .wav file."
    | _ => 
        IO.println "Usage: lake exe lean_audio <path_to_wav>"
