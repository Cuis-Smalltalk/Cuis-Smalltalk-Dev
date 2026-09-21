'From Cuis7.9 [latest update: #8207] on 21 September 2026 at 9:57:29 am'!

!Object methodsFor: 'object serialization' stamp: 'jmv 21/Sep/2026, 12:51:38 pm (UTC)'!
!methodProperties: {#libraryName -> #MinimalKernel06ObjectSerialization}!
readDataFrom: aDataStream size: varsOnDisk
	"Fill in the fields of self based on the contents of aDataStream.  Return self.
	 Read in the instance-variables written by Object>>storeDataOn:.
	 NOTE: This method must send beginReference: before reading any objects from aDataStream that might reference it.
	 Allow aDataStream to have fewer inst vars.  See SmartRefStream."
	| cntInstVars cntIndexedVars |

	self class isVariable
		ifTrue: [
			cntInstVars := self class instSize.
			cntIndexedVars := varsOnDisk - cntInstVars.
			cntIndexedVars < 0 ifTrue: [
				self error: 'Class has changed too much.  Define a convertxxx method' ]]
		ifFalse: [
			cntIndexedVars := 0.
			"If new ivars added, not on disk, ignore them.
			If ivars removed, present on disk but not in system, read them and ignore them."
			cntInstVars := varsOnDisk min: self class instSize ].

	aDataStream beginReference: self.
	1 to: cntInstVars do:
		[:i | self instVarAt: i put: aDataStream next].
	"Ignore any ivars that were removed (but presaent on disk)"
	self class isVariable ifFalse: [
		varsOnDisk - cntInstVars timesRepeat: [ aDataStream next ]].
	1 to: cntIndexedVars do:
		[:i | self basicAt: i put: aDataStream next].
	"Total number read MUST be equal to varsOnDisk!!"
	^ self	"If we ever return something other than self, fix calls 
			on (super readDataFrom: aDataStream size: anInteger)"! !

