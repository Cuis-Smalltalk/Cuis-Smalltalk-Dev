'From Cuis7.1 [latest update: #6511] on 10 July 2024 at 3:19:27 pm'!

!Symbol methodsFor: 'testing' stamp: 'jmv 7/10/2024 15:13:50'!
isValidGlobalName

	^self notEmpty and: [
		self first canBeGlobalVarInitial and: [
			self allSatisfy: [ :character | character isValidInIdentifiers ]]].
! !


!UnicodeSymbol methodsFor: 'testing' stamp: 'jmv 7/10/2024 15:13:58'!
isValidGlobalName

	^self notEmpty and: [
		self first canBeGlobalVarInitial and: [
			self allSatisfy: [ :character | character isValidInIdentifiers ]]].
! !


!ClassBuilder methodsFor: 'validation' stamp: 'jmv 7/10/2024 15:15:20'!
validateClassName: aSymbol
	"Validate the new class name"

	aSymbol isValidGlobalName ifFalse: [
		self error: 'Invalid class name'.
		^false].
	Smalltalk at: aSymbol ifPresent:[:old|
		(old isKindOf: Behavior) ifFalse:[
			self notify: aSymbol asText allBold, 
						' already exists!!\Proceed will store over it.' withNewLines]].
	^true! !

!methodRemoval: ClassBuilder #doesClassNameStartWithUppercase: stamp: 'jmv 7/10/2024 15:17:57'!
ClassBuilder removeSelector: #doesClassNameStartWithUppercase:!
