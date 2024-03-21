'From Cuis6.3 [latest update: #6287] on 21 March 2024 at 11:38:30 am'!

!CharacterScanner methodsFor: 'private' stamp: 'jmv 3/21/2024 11:36:18'!
setFont
	| attributes |
	"Set the font and other emphasis.
	In fact set actual ParagraphStyle (or nil), alignment, font and emphasis"

	self setActualFont: (text fontAt: lastIndex default: defaultFont).
	attributes := text attributesAt: lastIndex.
	text
		withAttributeValues: attributes
		do: [ :familyNameInText :pointSizeInText :emphasisInText :colorInText :alignmentInText :characterStyleInText :paragraphStyleInText :backgroundColorInText |
			colorInText ifNotNil: [ self textColor: colorInText ].
			self backgroundColor: backgroundColorInText.
			alignment := alignmentInText.
			paragraphStyle := paragraphStyleInText ].
	
	"Hardcoded color for TextAction"
	attributes do: [ :attribute |
		attribute forTextActionInfoDo: [ :info |
			self textColor: TextAction textActionColor ]].

	"Install various parameters from the font."
	spaceWidth := font widthOf: Character space.
	tabWidth := font pointSize * 3.! !

!CharacterScanner methodsFor: 'stop conditions' stamp: 'jmv 3/21/2024 11:37:01'!
tabDestX
	"This is the basic method of adjusting destX for a tab."

	^paragraphStyle
		ifNotNil: [
			paragraphStyle
				nextTabXFrom: destX
				leftMargin: leftMargin
				rightMargin: rightMargin ]
		ifNil: [
			(tabCount+1 * tabWidth max: destX) min: rightMargin ].! !


!PreferenceSet class reorganize!
('sys data' iconicHaloSpecifications machineDefault machineSlow machineSmalltalk80 prefFont prefGui prefProgramming prefSystem)
('sys preference' init initProgramming installDefault:in: installHaloPreferencesWith: installMiscPreferences sysPreferences)
('sys fonts' setDefaultFont: setDefaultFont:spec:)
('sys event handlers' defaultFontSize: machine: pointer:)
('services' openPreferencesInspector readAndApplyUserPrefs saveUserPrefs)
!

