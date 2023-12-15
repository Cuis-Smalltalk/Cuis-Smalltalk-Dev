'From Cuis6.0 [latest update: #6122] on 15 December 2023 at 4:39:55 pm'!

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/15/2023 15:22:42'!
demoAncientEgyptian
	"
	UnicodeString demoAncientEgyptian.
	"
	| utf8 fontFamily |
	utf8 :=  #[240 147 128 128 240 147 133 184 240 147 137 128 240 147 144 174 240 147 128 144 240 147 128 145 240 147 128 146 32 240 147 131 176 240 147 131 177 240 147 131 178 240 147 131 179 240 147 131 180 240 147 131 181 240 147 131 182 32 240 147 130 128 32 240 147 133 184 240 147 133 185 240 147 133 186 240 147 133 187 32 240 147 133 189 32 240 147 133 190 240 147 133 191 32 240 147 137 160 240 147 137 161 240 147 137 162 240 147 137 163 240 147 137 164 32 240 147 134 178 240 147 129 183 240 147 143 164 240 147 142 159 240 147 128 128 240 147 129 144 240 147 143 165 240 147 131 128 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 133 177 240 147 128 148 240 147 136 150 240 147 140 177 240 147 133 147 240 147 142 155 240 147 135 139 240 147 135 139 240 147 143 143 240 147 129 144 240 147 144 141 240 147 130 139 240 147 139 180 240 147 130 157 240 147 142 155 240 147 139 169 240 147 137 148 240 147 138 170 240 147 143 155 240 147 139 180 240 147 144 160 240 147 132 191 240 147 130 139 240 147 143 143 240 147 140 151 240 147 128 129 240 147 140 183 240 147 130 157 240 147 143 143 240 147 143 173 240 147 143 155 240 147 135 190 240 147 143 143 240 147 133 147 240 147 133 177 240 147 128 128 240 147 129 144 240 147 143 170 240 147 131 128 240 147 140 162 240 147 140 162 240 147 136 150 240 147 136 150 240 147 143 155].

	fontFamily := FontFamily readIfNeeded: DirectoryEntry trueTypeFontsDirectory / 'NotoEgyptianHieroglyphs'.
	(Text string: (UnicodeString fromUtf8Bytes: utf8) attributes: {TextFontFamily familyName: fontFamily familyName. TextFontSize pointSize: 60})
		editLabel: 'Ancient Egyptian Hieroglyphs!!'.! !

!UnicodeString class methodsFor: 'unicode demo' stamp: 'jmv 12/15/2023 15:22:49'!
demoChinese
	"
	UnicodeString demoChinese
	"
	| utf8 fontFamily |
	utf8 :=  #[230 156 137 228 187 163 229 143 139 230 175 146 228 186 139 230 177 151 229 155 179 232 166 143 230 156 128 232 179 135 228 184 141 228 191 130 233 153 186 233 155 134 232 168 152 231 180 176 230 132 155 233 159 191 228 187 138 227 128 130 229 155 163 230 131 133 232 168 152 233 152 170 229 136 182 229 185 180 229 135 186 230 144 186 229 164 167 229 134 133 228 186 149 230 138 149 230 152 168 227 128 130 229 155 179 232 173 183 230 131 133 229 185 179 229 182 139 230 155 180 229 146 140 231 155 174 229 139 157 228 188 129 231 167 129 229 188 181 230 173 162 232 131 189 227 128 130 229 140 150 230 141 156 232 184 143 228 191 157 232 145 151 230 156 159 229 163 176 230 151 165 232 138 184 229 173 166 229 156 176 228 187 178 233 161 158 230 151 165 231 168 189 230 150 135 228 186 186 227 128 130 230 180 187 230 131 145 230 159 187 230 162 168 229 155 179 232 166 167 229 149 143 228 184 142 230 137 147 231 168 174 229 160 130 233 171 152 233 150 139 231 167 128 230 173 162 231 180 153 232 166 154 227 128 130 229 155 178 232 169 177 228 189 143 230 179 163 230 154 174 230 132 155 229 144 190 229 139 157 233 164 168 229 143 165 230 152 168 232 152 135 229 174 159 227 128 130 233 150 139 230 142 178 230 181 170 229 160 177 233 128 177 229 187 131 230 150 135 229 173 166 233 128 154 230 157 165 229 133 136 230 160 170 230 156 128 229 189 147 230 173 162 228 188 154 232 178 171 230 136 166 231 190 169 227 128 130 232 168 128 230 177 154 230 156 172 230 166 130 231 166 129 230 156 171 229 160 128 230 148 191 231 154 132 229 164 167 231 148 179 229 164 154 230 159 187 232 151 164 227 128 130 233 133 184 230 132 143 229 177 177 230 159 187 230 150 173 232 191 171 229 155 179 230 163 132 231 139 172 232 165 191 233 155 187 230 150 135 229 174 182 228 184 128 229 155 158 229 183 165 229 144 140 227 128 130].

	fontFamily := FontFamily readIfNeeded: DirectoryEntry trueTypeFontsDirectory / 'KiwiMaru'.
	(Text string: (UnicodeString fromUtf8Bytes: utf8) attributes: {TextFontFamily familyName: fontFamily familyName. TextFontSize pointSize: 24})
		editLabel: 'Chinese'.! !


!FontFamily class methodsFor: 'ui' stamp: 'jmv 12/15/2023 15:26:59'!
promptUser: currentNameOrNil
	"
	FontFamily promptUser
	FontFamily promptUser: FontFamily defaultFamilyName
	"
	"Present a menu of available fonts, and if one is chosen, return it.
	Otherwise return nil."

	| menu answer |
	menu := MenuMorph new.
	menu defaultTarget: menu.
	menu addLabel: '-- Installed Fonts --'.
	menu addLine.
	FontFamily familyNames do: [ :fontName |
		menu
			add: (fontName=currentNameOrNil ifTrue: ['<on>'] ifFalse: ['<off>']), fontName
			target: menu
			action: #modalSelection:
			argument: {#fontName. fontName} ].

	menu addLine.
	menu addLabel: '-- Available to install --'.
	menu addLine.
	(DirectoryEntry trueTypeFontsDirectory
		directories sorted: [ :a :b | a name < b name ]) do: [ :dir | | subDirs |
			subDirs := dir directories sorted: [ :a :b | a name < b name ].
			subDirs isEmpty
				ifTrue: [
					self addFontDir: dir to: menu current: currentNameOrNil ]
				ifFalse: [ | submenu anyAdded |
					submenu := MenuMorph new defaultTarget: menu.
					anyAdded := false.
					subDirs do: [ :subdir |
						anyAdded := anyAdded | (self addFontDir: subdir to: submenu current: currentNameOrNil) ].
					anyAdded ifTrue: [
						menu add: dir name, '... ' subMenu: submenu ]]].
	menu lastSubmorph class == MenuLineMorph ifTrue: [
		3 timesRepeat: [menu lastSubmorph delete ]].

	answer := menu invokeModal.
	answer ifNil: [ ^nil ].
	answer first = #fontDirectory
		ifTrue: [ TrueTypeFontFamily readAllTrueTypeFontsIn: answer third ].
	^FontFamily familyNamed: answer second! !


!TrueTypeFontFamily class methodsFor: 'instance creation' stamp: 'jmv 12/15/2023 15:27:11'!
readAdditionalFonts

	TrueTypeFontFamily readAllTrueTypeFontsIn: DirectoryEntry trueTypeFontsDirectory.! !


!Sample09Clock methodsFor: 'initialization' stamp: 'jmv 12/15/2023 15:27:18'!
initialize

	super initialize.
	self addMorph: (hourHand := Sample09ClockHourHand new).
	self addMorph: (minuteHand := Sample09ClockMinuteHand new).
	self addMorph: (secondHand := Sample09ClockSecondHand new).

	(FontFamily familyName: 'Alex Brush' pointSize: 16) ifNil: [
		TrueTypeFontFamily readTrueTypeFontEntry:
			DirectoryEntry trueTypeFontsDirectory / 'AlexBrush' // 'AlexBrush-Regular.ttf' ]! !


!TTFontDescription class methodsFor: 'instance creation' stamp: 'jmv 12/15/2023 16:39:28'!
readTrueTypeFontsIn: aDirectoryEntry
	"
	TTFontDescription read: DirectoryEntry trueTypeFontsDirectory
	"
	| read |
	read := OrderedDictionary new.
	aDirectoryEntry
		allFilesDo: [ :file | | ttf |
			ttf := TTFontReader readTTFFrom: file binaryContents.
			ttf folderName: file parent name.
			self add: ttf to: read ]
		matches: [ :file | '*.ttf' match: file name ].
	aDirectoryEntry
		allFilesDo: [ :file | | ttc |
			ttc := TTFontReader readTTCFrom: file binaryContents.
			ttc do: [ :eachTtf |
				eachTtf folderName: file parent name.
				self add: eachTtf to: read ]]
		matches: [ :file | '*.ttc' match: file name ].
	Descriptions ifNil: [ Descriptions := Dictionary new ].
	read keysAndValuesDo: [ :familyName :emphasisDict |
		emphasisDict keysAndValuesDo: [ :emphasis :ttf |
			(Descriptions at: familyName ifAbsentPut: [ Dictionary new]) at: emphasis put: ttf ]].
	^read keys.! !

