'From Cuis 4.2 of 25 July 2013 [latest update: #2134] on 14 December 2014 at 2:49:53.940571 pm'!

!CompiledMethod methodsFor: 'comparing' stamp: 'jmv 12/14/2014 14:49'!
hash
	"CompiledMethod>>#= compares code, i.e. same literals and same bytecode.
	 So we look at the header, methodClass and some bytes between initialPC and endPC,
	 but /not/ the selector because the equal method does not compare selectors.
	 Note that we must override ByteArray>hash which looks at all bytes of the receiver.
	 Using bytes from the pointer part of a COmpiledmethod can lead to a variable hash
	 if and when when the GC moves literals in the receiver.
	jmv: As I made #= not to compare for methodClass, remove it from here."
	| initialPC endPC hash |
	initialPC := self initialPC.
	endPC := self endPC.
	hash := self species hash + self header + initialPC + endPC "+ self methodClass hash" bitAnd: 16rFFFFFFF.
	"sample approximately 20 bytes"
	initialPC to: endPC by: (endPC - initialPC // 20 max: 1) do: [ :i |
		hash := hash + (self at: i)].
	^hash

	"(CompiledMethod>>#hash) hash"! !

