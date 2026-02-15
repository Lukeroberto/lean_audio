structure SignalStats where
  size : Nat

  min : Float
  max : Float
  deriving Repr -- Allows printing

def stats (signal : FloatArray) : SignalStats :=
  if signal.size > 0 then
    -- Convert Nat size to Float for potential later use
    let initial := signal.get! 0
    
    -- Using the standard min/max functions
    let (minVal, maxVal) := signal.foldl (init := (initial, initial)) 
      fun (mn, mx) val => (min mn val, max mx val)
    
    {
      size := signal.size
      min  := minVal
      max  := maxVal
    }
  else
    {
      size := 0
      min := 0
      max := 0
  }


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
  
  -- Fix 1: Use FloatArray.empty
  let mut floats := FloatArray.empty

  for i in [0 : numSamples] do
    let low  := bytes.get! (44 + 2 * i)
    let high := bytes.get! (44 + 2 * i + 1)
    
    let val := (high.toUInt16 <<< 8) ||| low.toUInt16
    
    let signedVal : Float := 
      if val >= 32768 then 
        (val.toFloat - 65536.0) 
      else 
        val.toFloat
    
    floats := floats.push (signedVal / 32768.0)
    
  return floats

def main (args : List String) : IO Unit := do
  match args with 
    | [path] =>
        if ← isValidWav path then
          IO.println s!"Loading {path}..."
          let signal ← loadWavToFloatArray path
          let sigStats := stats signal
          IO.println s!"Signal stats: {repr sigStats}"
        else
          IO.println "Invalid file path or not a .wav file."
    | _ => 
        IO.println "Usage: lake exe lean_audio <path_to_wav>"
