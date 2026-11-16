'From Cuis7.9 [latest update: #8364] on 16 November 2026 at 12:14:49 pm'!

!CompiledMethod methodsFor: 'source code management' stamp: 'jmv 16/Nov/2026, 3:12:03 pm (UTC)'!
sourceOrDecompiled
	"Answer source code. If not available, answer decompiled code."

	^self getSourceFromFile ifNil: [ self decompileString ]! !


!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 16/Nov/2026, 3:13:04 pm (UTC)'!
sourceCodeAt: selector

	^ (self methodDict at: selector) sourceOrDecompiled! !

!Behavior methodsFor: 'accessing method dictionary' stamp: 'jmv 16/Nov/2026, 3:13:08 pm (UTC)'!
sourceCodeAt: selector ifAbsent: aBlock

	^ (self methodDict at: selector ifAbsent: [^ aBlock value]) sourceOrDecompiled! !

