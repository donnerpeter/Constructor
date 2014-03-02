module Constructor.Util where
import qualified Debug.Trace as DT

trace msg x = DT.traceShow msg x
traceIt msg x = trace (msg, x) x