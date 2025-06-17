'From Cuis7.5 [latest update: #7263] on 9 June 2025 at 4:41:03 pm'!

!InstructionStream reorganize!
('testing' mapsPcTo: willBlockReturn willJump willJumpIfFalse willJumpIfTrue willJustPop willReallyStore willReturn willReturnTopFromMethod willSend willStore willStorePop)
('decoding' atEnd atEnd: interpret interpretJump interpretJumpIfCond interpretNextInstructionFor: skipCallPrimitive)
('scanning' addSelectorTo: firstByte followingByte followingBytecode followingPc fourthByte method movePcForward nextByte nextInstruction pc peekInstruction previousPc scanFor: secondByte selectorToSendOrSelf skipBackBeforeJump thirdByte)
('private' method:pc: nextPc: pc: unusedBytecode:at:)
('debugger access' abstractPC debuggerMap)
('decoding - private - v3 plus closures' interpretNextV3ClosuresInstructionFor: interpretV3ClosuresExtension:in:for: interpretV3Extension:in:for: interpretV3Jump interpretV3JumpIfCond)
('decoding - private - sista v1' interpretNext2ByteSistaV1Instruction:for:extA:extB:startPC: interpretNext3ByteSistaV1Instruction:for:extA:extB:startPC: interpretNextSistaV1InstructionFor: interpretSistaV1ExtendedPush:for: interpretSistaV1Jump interpretSistaV1JumpIfCond)
!

