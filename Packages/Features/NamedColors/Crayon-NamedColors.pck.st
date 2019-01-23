'From Cuis 5.0 of 7 November 2016 [latest update: #3319] on 12 May 2018 at 5:44:25 pm'!
'Description Crayon Colors from
	http://www.colourlovers.com/web/blog/2008/04/22/all-120-crayon-names-color-codes-and-fun-facts'!
!provides: 'Crayon-NamedColors' 1 1!
!requires: 'Color-Extras' 1 4 nil!



!Color class methodsFor: '*crayon-namedcolors' stamp: 'KenD 5/28/2015 15:36'!
crayonColorDictionary
	"Answer a dictionary of Crayon color names.
	http://www.colourlovers.com/web/blog/2008/04/22/all-120-crayon-names-color-codes-and-fun-facts"
" 
	self crayonColorDictionary  explore.
	
To reset the Color name->color dictionary:
 	Color setColorNamesDict: (Color crayonColorDictionary).
."
	| crayonColorDict |

	crayonColorDict := self crayonColors1 addAll: self crayonColors2; 
		addAll: self crayonColors3;  yourself.
		
	^crayonColorDict
! !

!Color class methodsFor: '*crayon-namedcolors' stamp: 'KenD 5/28/2015 15:33'!
crayonColors1
	"Answer a dictionary of XKCD color names survey name to Color objects."

	^Dictionary new
    at: #'Mahogany' put: (Color r: 205 g: 74 b: 74 range: 255);
    at: #'Fuzzy Wuzzy Brown' put: (Color r: 204 g: 102 b: 102 range: 255);
    at: #'Chestnut' put: (Color r: 188 g: 93 b: 88 range: 255);
    at: #'Red Orange' put: (Color r: 255 g: 83 b: 73 range: 255);
    at: #'Sunset Orange' put: (Color r: 253 g: 94 b: 83 range: 255);
    at: #'Bittersweet' put: (Color r: 253 g: 124 b: 110 range: 255);
    at: #'Melon' put: (Color r: 253 g: 188 b: 180 range: 255);
    at: #'Outrageous Orange' put: (Color r: 255 g: 110 b: 74 range: 255);
    at: #'Vivid Tangerine' put: (Color r: 255 g: 160 b: 137 range: 255);
    at: #'Burnt Sienna' put: (Color r: 234 g: 126 b: 93 range: 255);
    at: #'Brown' put: (Color r: 180 g: 103 b: 77 range: 255);
    at: #'Sepia' put: (Color r: 165 g: 105 b: 79 range: 255);
    at: #'Orange' put: (Color r: 255 g: 117 b: 56 range: 255);
    at: #'Burnt Orange' put: (Color r: 255 g: 127 b: 73 range: 255);
    at: #'Copper' put: (Color r: 221 g: 148 b: 117 range: 255);
    at: #'Mango Tango' put: (Color r: 255 g: 130 b: 67 range: 255);
    at: #'Atomic Tangerine' put: (Color r: 255 g: 164 b: 116 range: 255);
    at: #'Beaver' put: (Color r: 159 g: 129 b: 112 range: 255);
    at: #'Antique Brass' put: (Color r: 205 g: 149 b: 117 range: 255);
    at: #'Desert Sand' put: (Color r: 239 g: 205 b: 184 range: 255);
    at: #'Raw Sienna' put: (Color r: 214 g: 138 b: 89 range: 255);
yourself! !

!Color class methodsFor: '*crayon-namedcolors' stamp: 'KenD 5/28/2015 15:34'!
crayonColors2
	"Answer a dictionary of XKCD color names survey name to Color objects."

	^Dictionary new
     at: #'Tumbleweed' put: (Color r: 222 g: 170 b: 136 range: 255);
    at: #'Tan' put: (Color r: 250 g: 167 b: 108 range: 255);
    at: #'Peach' put: (Color r: 255 g: 207 b: 171 range: 255);
    at: #'Macaroni and Cheese' put: (Color r: 255 g: 189 b: 136 range: 255);
    at: #'Apricot' put: (Color r: 253 g: 217 b: 181 range: 255);
    at: #'Neon Carrot' put: (Color r: 255 g: 163 b: 67 range: 255);
    at: #'Almond' put: (Color r: 239 g: 219 b: 197 range: 255);
    at: #'Yellow Orange' put: (Color r: 255 g: 182 b: 83 range: 255);
    at: #'Gold' put: (Color r: 231 g: 198 b: 151 range: 255);
    at: #'Shadow' put: (Color r: 138 g: 121 b: 93 range: 255);
    at: #'Banana Mania' put: (Color r: 250 g: 231 b: 181 range: 255);
    at: #'Sunglow' put: (Color r: 255 g: 207 b: 72 range: 255);
    at: #'Goldenrod' put: (Color r: 252 g: 217 b: 117 range: 255);
    at: #'Dandelion' put: (Color r: 253 g: 219 b: 109 range: 255);
    at: #'Yellow' put: (Color r: 252 g: 232 b: 131 range: 255);
    at: #'Green Yellow' put: (Color r: 240 g: 232 b: 145 range: 255);
    at: #'Spring Green' put: (Color r: 236 g: 234 b: 190 range: 255);
    at: #'Olive Green' put: (Color r: 186 g: 184 b: 108 range: 255);
    at: #'Laser Lemon' put: (Color r: 253 g: 252 b: 116 range: 255);
    at: #'Unmellow Yellow' put: (Color r: 253 g: 252 b: 116 range: 255);
    at: #'Canary' put: (Color r: 255 g: 255 b: 153 range: 255);
    at: #'Yellow Green' put: (Color r: 197 g: 227 b: 132 range: 255);
    at: #'Inch Worm' put: (Color r: 178 g: 236 b: 93 range: 255);
    at: #'Asparagus' put: (Color r: 135 g: 169 b: 107 range: 255);
    at: #'Granny Smith Apple' put: (Color r: 168 g: 228 b: 160 range: 255);
    at: #'Electric Lime' put: (Color r: 29 g: 249 b: 20 range: 255);
    at: #'Screamin Green' put: (Color r: 118 g: 255 b: 122 range: 255);
    at: #'Fern' put: (Color r: 113 g: 188 b: 120 range: 255);
    at: #'Forest Green' put: (Color r: 109 g: 174 b: 129 range: 255);
    at: #'Sea Green' put: (Color r: 159 g: 226 b: 191 range: 255);
    at: #'Green' put: (Color r: 28 g: 172 b: 120 range: 255);
    at: #'Mountain Meadow' put: (Color r: 48 g: 186 b: 143 range: 255);
    at: #'Shamrock' put: (Color r: 69 g: 206 b: 162 range: 255);
    at: #'Jungle Green' put: (Color r: 59 g: 176 b: 143 range: 255);
    at: #'Caribbean Green' put: (Color r: 28 g: 211 b: 162 range: 255);
    at: #'Tropical Rain Forest' put: (Color r: 23 g: 128 b: 109 range: 255);
    at: #'Pine Green' put: (Color r: 21 g: 128 b: 120 range: 255);
    at: #'Robin Egg Blue' put: (Color r: 31 g: 206 b: 203 range: 255);
    at: #'Aquamarine' put: (Color r: 120 g: 219 b: 226 range: 255);
    at: #'Turquoise Blue' put: (Color r: 119 g: 221 b: 231 range: 255);
    at: #'Sky Blue' put: (Color r: 128 g: 218 b: 235 range: 255);
    at: #'Outer Space' put: (Color r: 65 g: 74 b: 76 range: 255);
    at: #'Blue Green' put: (Color r: 25 g: 158 b: 189 range: 255);
    at: #'Pacific Blue' put: (Color r: 28 g: 169 b: 201 range: 255);
    at: #'Cerulean' put: (Color r: 29 g: 172 b: 214 range: 255);
    at: #'Cornflower' put: (Color r: 154 g: 206 b: 235 range: 255);
    at: #'Midnight Blue' put: (Color r: 26 g: 72 b: 118 range: 255);
    at: #'Navy Blue' put: (Color r: 25 g: 116 b: 210 range: 255);
    at: #'Denim' put: (Color r: 43 g: 108 b: 196 range: 255);
    at: #'Blue' put: (Color r: 31 g: 117 b: 254 range: 255);
    at: #'Periwinkle' put: (Color r: 197 g: 208 b: 230 range: 255);
    at: #'Cadet Blue' put: (Color r: 176 g: 183 b: 198 range: 255);
    at: #'Indigo' put: (Color r: 93 g: 118 b: 203 range: 255);
    at: #'Wild Blue Yonder' put: (Color r: 162 g: 173 b: 208 range: 255);
    at: #'Manatee' put: (Color r: 151 g: 154 b: 170 range: 255);
    at: #'Blue Bell' put: (Color r: 173 g: 173 b: 214 range: 255);
    at: #'Blue Violet' put: (Color r: 115 g: 102 b: 189 range: 255);
    at: #'Purple Heart' put: (Color r: 116 g: 66 b: 200 range: 255);
    at: #'Royal Purple' put: (Color r: 120 g: 81 b: 169 range: 255);
    at: #'Purple Mountains’ Majesty' put: (Color r: 157 g: 129 b: 186 range: 255);
    at: #'Violet (Purple)' put: (Color r: 146 g: 110 b: 174 range: 255);
    at: #'Wisteria' put: (Color r: 205 g: 164 b: 222 range: 255);
    at: #'Vivid Violet' put: (Color r: 143 g: 80 b: 157 range: 255);
    at: #'Fuchsia' put: (Color r: 195 g: 100 b: 197 range: 255);
    at: #'Shocking Pink' put: (Color r: 251 g: 126 b: 253 range: 255);
    at: #'Pink Flamingo' put: (Color r: 252 g: 116 b: 253 range: 255);
    at: #'Plum' put: (Color r: 142 g: 69 b: 133 range: 255);
    at: #'Hot Magenta' put: (Color r: 255 g: 29 b: 206 range: 255);
    at: #'Purple Pizzazz' put: (Color r: 255 g: 29 b: 206 range: 255);
    at: #'Razzle Dazzle Rose' put: (Color r: 255 g: 72 b: 208 range: 255);
    at: #'Orchid' put: (Color r: 230 g: 168 b: 215 range: 255);
    at: #'Red Violet' put: (Color r: 192 g: 68 b: 143 range: 255);
    at: #'Eggplant' put: (Color r: 110 g: 81 b: 96 range: 255);
    at: #'Cerise' put: (Color r: 221 g: 68 b: 146 range: 255);
    at: #'Wild Strawberry' put: (Color r: 255 g: 67 b: 164 range: 255);
yourself! !

!Color class methodsFor: '*crayon-namedcolors' stamp: 'KenD 5/28/2015 15:35'!
crayonColors3
	"Answer a dictionary of XKCD color names survey name to Color objects."

	^Dictionary new
    at: #'Magenta' put: (Color r: 246 g: 100 b: 175 range: 255);
    at: #'Lavender' put: (Color r: 252 g: 180 b: 213 range: 255);
    at: #'Cotton Candy' put: (Color r: 255 g: 188 b: 217 range: 255);
    at: #'Violet Red' put: (Color r: 247 g: 83 b: 148 range: 255);
    at: #'Carnation Pink' put: (Color r: 255 g: 170 b: 204 range: 255);
    at: #'Razzmatazz' put: (Color r: 227 g: 37 b: 107 range: 255);
    at: #'Piggy Pink' put: (Color r: 253 g: 215 b: 228 range: 255);
    at: #'Jazzberry Jam' put: (Color r: 202 g: 55 b: 103 range: 255);
    at: #'Blush' put: (Color r: 222 g: 93 b: 131 range: 255);
    at: #'Tickle Me Pink' put: (Color r: 252 g: 137 b: 172 range: 255);
    at: #'Pink Sherbet' put: (Color r: 247 g: 128 b: 161 range: 255);
    at: #'Maroon' put: (Color r: 200 g: 56 b: 90 range: 255);
    at: #'Red' put: (Color r: 238 g: 32 b: 77 range: 255);
    at: #'Radical Red' put: (Color r: 255 g: 73 b: 108 range: 255);
    at: #'Mauvelous' put: (Color r: 239 g: 152 b: 170 range: 255);
    at: #'Wild Watermelon' put: (Color r: 252 g: 108 b: 133 range: 255);
    at: #'Scarlet' put: (Color r: 252 g: 40 b: 71 range: 255);
    at: #'Salmon' put: (Color r: 255 g: 155 b: 170 range: 255);
    at: #'Brick Red' put: (Color r: 203 g: 65 b: 84 range: 255);
    at: #'White' put: (Color r: 237 g: 237 b: 237 range: 255);
    at: #'Timberwolf' put: (Color r: 219 g: 215 b: 210 range: 255);
    at: #'Silver' put: (Color r: 205 g: 197 b: 194 range: 255);
    at: #'Gray' put: (Color r: 149 g: 145 b: 140 range: 255);
    at: #'Black' put: (Color r: 35 g: 35 b: 35 range: 255);
yourself! !
