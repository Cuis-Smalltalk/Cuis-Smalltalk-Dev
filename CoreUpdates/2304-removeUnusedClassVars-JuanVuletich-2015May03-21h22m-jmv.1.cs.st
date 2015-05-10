'From Cuis 4.2 of 25 July 2013 [latest update: #2303] on 3 May 2015 at 9:42:25.326244 pm'!
!classDefinition: #ProtocolBrowser category: #'Tools-Browser'!
MessageSet subclass: #ProtocolBrowser
	instanceVariableNames: 'selectedClass selectedSelector '
	classVariableNames: 'TextMenu '
	poolDictionaries: ''
	category: 'Tools-Browser'!
!classDefinition: #TimeProfileBrowser category: #'Tools-Profiling'!
MessageSet subclass: #TimeProfileBrowser
	instanceVariableNames: 'selectedClass selectedSelector block tally '
	classVariableNames: 'TextMenu '
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #DebuggerMethodMapForClosureCompiledMethods category: #'Tools-Debugger'!
DebuggerMethodMap subclass: #DebuggerMethodMapForClosureCompiledMethods
	instanceVariableNames: 'blockExtentsToTempRefs startpcsToTempRefs '
	classVariableNames: 'FirstTime '
	poolDictionaries: ''
	category: 'Tools-Debugger'!
!classDefinition: #ImageReadWriter category: #'Graphics-Files'!
Object subclass: #ImageReadWriter
	instanceVariableNames: 'stream '
	classVariableNames: 'ImageNotStoredSignal MagicNumberErrorSignal '
	poolDictionaries: ''
	category: 'Graphics-Files'!
!classDefinition: #ParseNode category: #'Compiler-ParseNodes'!
Object subclass: #ParseNode
	instanceVariableNames: 'comment pc '
	classVariableNames: 'Bfp BtpLong CodeBases CodeLimits DblExtDoAll Dup EndMethod EndRemote Jmp JmpLimit JmpLong LdFalse LdInstLong LdInstType LdLitIndType LdLitType LdMinus1 LdNil LdSelf LdSuper LdTempType LdThisContext LdTrue LoadLong LongLongDoAll NodeFalse NodeNil NodeSelf NodeSuper NodeThisContext NodeTrue Pop Send SendLimit SendLong SendLong2 SendPlus SendType ShortStoP StdLiterals StdSelectors StdVariables Store StorePop '
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #MessageNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #MessageNode
	instanceVariableNames: 'receiver selector precedence special arguments sizes equalNode caseErrorNode originalReceiver originalSelector originalArguments '
	classVariableNames: 'MacroEmitters MacroPrinters MacroSelectors MacroSizers MacroTransformers StdTypers ThenFlag '
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #MessageNode category: #'Compiler-ParseNodes'!
ParseNode subclass: #MessageNode
	instanceVariableNames: 'receiver selector precedence special arguments sizes equalNode caseErrorNode originalReceiver originalSelector originalArguments'
	classVariableNames: 'MacroEmitters MacroPrinters MacroSelectors MacroSizers MacroTransformers'
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #ParseNode category: #'Compiler-ParseNodes'!
Object subclass: #ParseNode
	instanceVariableNames: 'comment pc'
	classVariableNames: 'Bfp BtpLong CodeBases CodeLimits DblExtDoAll Dup EndMethod EndRemote Jmp JmpLimit JmpLong LdFalse LdInstType LdLitIndType LdLitType LdMinus1 LdNil LdSelf LdSuper LdTempType LdThisContext LdTrue LoadLong NodeFalse NodeNil NodeSelf NodeSuper NodeThisContext NodeTrue Pop Send SendLimit SendLong SendLong2 SendPlus SendType ShortStoP StdLiterals StdSelectors StdVariables Store StorePop'
	poolDictionaries: ''
	category: 'Compiler-ParseNodes'!
!classDefinition: #ImageReadWriter category: #'Graphics-Files'!
Object subclass: #ImageReadWriter
	instanceVariableNames: 'stream'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Files'!
!classDefinition: #DebuggerMethodMapForClosureCompiledMethods category: #'Tools-Debugger'!
DebuggerMethodMap subclass: #DebuggerMethodMapForClosureCompiledMethods
	instanceVariableNames: 'blockExtentsToTempRefs startpcsToTempRefs'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Debugger'!
!classDefinition: #TimeProfileBrowser category: #'Tools-Profiling'!
MessageSet subclass: #TimeProfileBrowser
	instanceVariableNames: 'selectedClass selectedSelector block tally'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Profiling'!
!classDefinition: #ProtocolBrowser category: #'Tools-Browser'!
MessageSet subclass: #ProtocolBrowser
	instanceVariableNames: 'selectedClass selectedSelector'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Tools-Browser'!
