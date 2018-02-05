#/bin/sh

for i in {0..255}; do stack exec ghc -- Backend/Trace/Eval/Trace.hs -e "genSingleHyp $i \"Backend/Trace/Data/inputs.dat\"" 2> Backend/Trace/Data/Hyps/Hypstrace_$i.dat > /dev/null; done
