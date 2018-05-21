SHELL := /bin/bash
.PHONY: help

help: ## This help message
	@echo "# Available targets"
	@echo ""
	@echo -e "$$(grep -hE '^\S+:.*##' $(MAKEFILE_LIST) | sed -e 's/:.*##\s*/:/' -e 's/^\(.\+\):\(.*\)/\\x1b[36m\1\\x1b[m:\2/' | column -c2 -t -s :)"
	@echo ""

check-naive-masking: ## Check naive masking properties of the masked SBOX through a symbolic heuristics
	@echo "Checking naive masking properties of MaskedSBOX through a heuristics"
	stack exec runhaskell -- Backend/MaskProp/Eval/EvalMasked.hs

qc-primitives: ## Functional verification of both masked and unmasked SBOX primitives with QuickCheck
	@echo "Quick check both masked and unmasked SBOX primitives"
	stack exec runhaskell -- Backend/CLaSH/Eval/EvalCheck.hs

prove-unmasked: ## Functional verification unmasked SBOX with SMT Solver
	@echo "Testing unmasked SBOX with SMT Solver"
	stack exec runhaskell -- Backend/SBV/Eval/EvalUnmasked.hs

prove-masked: ## Functional verification of the masked SBOX with SMT Solver
	@echo "Testing masked SBOX with SMT Solver"
	stack exec runhaskell -- Backend/SBV/Eval/EvalMasked.hs

syn-unmasked: ## Synthesize unmasked SBOX through CLASH
	@echo "Synthesizing unmasked SBOX through CLASH"
	stack exec clash -- --verilog Backend/CLaSH/Eval/EvalUnmasked.hs

syn-masked: ## Synthesize masked SBOX through CLaSH
	@echo "Synthesizing masked SBOX through CLaSH"
	stack exec clash -- --verilog Backend/CLaSH/Eval/EvalMasked.hs

generate-inputs: ## Generate inputs.dat for high-level simulation
	@echo "Generating inputs.dat for tracing"
	stack exec ghc -- Backend/Trace/Eval/Trace.hs -e "generateNInputs 10000" > Backend/Trace/Data/inputs.dat

# probe: ## Generate traces from inputs.dat
# 	@echo "Generating traces from inputs.dat"
# 	-stack exec ghc -- Backend/Trace/Debug.hs -e 'applyNInputs testNOMASKS    "Backend/Trace/Data/inputs.dat"' 2> Backend/Trace/Data/trace_NOMASKS.dat > /dev/null
# 	-stack exec ghc -- Backend/Trace/Debug.hs -e 'applyNInputs testWithFreshM "Backend/Trace/Data/inputs.dat"' 2> Backend/Trace/Data/trace_WithFreshM.dat > /dev/null
# 	-stack exec ghc -- Backend/Trace/Debug.hs -e 'applyNInputs testNoFreshM   "Backend/Trace/Data/inputs.dat"' 2> Backend/Trace/Data/trace_NoFreshM.dat > /dev/null
# 	-stack exec ghc -- Backend/Trace/Debug.hs -e 'applyNInputs testBrokenOsw  "Backend/Trace/Data/inputs.dat"' 2> Backend/Trace/Data/trace_BrokenOsw.dat > /dev/null
# 	-stack exec ghc -- Backend/Trace/Debug.hs -e 'applyNInputs testDepMasks   "Backend/Trace/Data/inputs.dat"' 2> Backend/Trace/Data/trace_DepMasks.dat > /dev/null

simul-traces: ## Generate high-level simulation of Primitives traces starting from inputs.dat
	@echo "Generating traces from inputs.dat"
	-stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'applyNInputs testNOMASKS    "Backend/Trace/Data/inputs.dat"' 2> Backend/Trace/Data/trace_NOMASKS.dat > /dev/null
	-stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'applyNInputs testWithFreshM "Backend/Trace/Data/inputs.dat"' 2> Backend/Trace/Data/trace_WithFreshM.dat > /dev/null
	-stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'applyNInputs testNoFreshM   "Backend/Trace/Data/inputs.dat"' 2> Backend/Trace/Data/trace_NoFreshM.dat > /dev/null
	-stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'applyNInputs testBrokenOsw  "Backend/Trace/Data/inputs.dat"' 2> Backend/Trace/Data/trace_BrokenOsw.dat > /dev/null
	-stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'applyNInputs testDepMaskHI  "Backend/Trace/Data/inputs.dat"' 2> Backend/Trace/Data/trace_DepMaskHI.dat > /dev/null
	-stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'applyNInputs testDepMaskLO  "Backend/Trace/Data/inputs.dat"' 2> Backend/Trace/Data/trace_DepMaskLO.dat > /dev/null

generate-sbox-inputs: ## Print the input of the SBOX, from inputs.dat and the (hardcoded) secret key
	@echo "Printing the SBOX input."
	stack exec ghc -- Backend/Trace/Eval/Trace.hs -e 'printSBoxIN "Backend/Trace/Data/inputs.dat"' > Backend/Trace/Data/sboxIN.dat

clean: ## Clean up folders from garbage
	rm -f *.hi *.o
	find . -name "*\.o" -print0 | xargs -0 -n 1 -J _ rm
	find . -name "*\.hi" -print0 | xargs -0 -n 1 -J _ rm

dist-clean: clean ## Cleanup and remove also the .dat files
	rm -f Backend/Trace/Data/*.dat

syn-keccak: ## Generate verilog for additional Keccak example
	stack exec clash -- --verilog Backend/CLaSH/Eval/Keccak/EvalKeccak.hs

syn-keccak-vhdl: ## Generate verilog for additional Keccak example
	stack exec clash -- --vhdl Backend/CLaSH/Eval/Keccak/EvalKeccak.hs

keccak-verilog.tar.gz:
	rm -rf verilog
	make syn-keccak
	tar cvzf $@ verilog

keccak-vhdl.tar.gz:
	rm -rf vhdl
	make syn-keccak-vhdl
	tar cvzf $@ vhdl

time-flow:
	time make prove-masked
	time make syn-masked
	time make check-naive-masking

prove-keccak: ## Prove correctness of Keccak three shares implementation through SMT
	@echo "Checking Keccak implementation through SMT"
	stack exec ghc -- Backend/SBV/Eval/Keccak/EvalKeccak.hs -e 'main'

check-hlsim-keccak:
	stack exec ghc -- Backend/CLaSH/Eval/Keccak/HLSimCheck.hs -e 'main'
