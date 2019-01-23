'From Cuis 5.0 of 7 November 2016 [latest update: #3319] on 12 May 2018 at 5:43:49 pm'!
'Description CSS2 color names come from an internet web standard by W3C:
      CSSColor Module Level 2
These have been subsumed by the larger CSS3 color name set.

	 http://www.w3.org/TR/css3-color/

See Class side:
	Color css2ColorDictionary explore.

To reset the Color name->color dictionary:
 	Color setColorNamesDict: (Color css2ColorDictionary).'!
!provides: 'CSS2-NamedColors' 1 1!
!requires: 'Cuis-Base' 42 1788 nil!
!requires: 'Color-Extras' 1 4 nil!
!requires: 'Graphics-Files-Additional' 1 nil nil!



!Color class methodsFor: '*css2-namedcolors' stamp: 'KenD 1/28/2013 19:34'!
css2ColorDictionary
	"Answer a dictionary of CSS2 compliant color names to Color objects."
"
	self css2ColorDictionary explore.
"

	^(self css2NamedColors1 addAll: self css2NamedColors2; yourself)
	! !

!Color class methodsFor: '*css2-namedcolors' stamp: 'KenD 1/28/2013 19:32'!
css2NamedColors1
	"Answer a dictionary of CSS2 compliant color names to Color objects.
	First half!!"

	^Dictionary new
		at: 'aliceblue' put: (Color r: 240 g: 248 b: 255 range: 255);
		at: 'antiquewhite' put: (Color r: 250 g: 235 b: 215 range: 255);
		at: 'aqua' put: (Color r: 0 g: 255 b: 255 range: 255);
		at: 'aquamarine' put: (Color r: 127 g: 255 b: 212 range: 255);
		at: 'azure' put: (Color r: 240 g: 255 b: 255 range: 255);
		at: 'beige' put: (Color r: 245 g: 245 b: 220 range: 255);
		at: 'bisque' put: (Color r: 255 g: 228 b: 196 range: 255);
		at: 'black' put: (Color r: 0 g: 0 b: 0 range: 255);
		at: 'blanchedalmond' put: (Color r: 255 g: 235 b: 205 range: 255);
		at: 'blue' put: (Color r: 0 g: 0 b: 255 range: 255);
		at: 'blueviolet' put: (Color r: 138 g: 43 b: 226 range: 255);
		at: 'brown' put: (Color r: 165 g: 42 b: 42 range: 255);
		at: 'burlywood' put: (Color r: 222 g: 184 b: 135 range: 255);
		at: 'cadetblue' put: (Color r: 95 g: 158 b: 160 range: 255);
		at: 'chartreuse' put: (Color r: 127 g: 255 b: 0 range: 255);
		at: 'chocolate' put: (Color r: 210 g: 105 b: 30 range: 255);
		at: 'coral' put: (Color r: 255 g: 127 b: 80 range: 255);
		at: 'cornflowerblue' put: (Color r: 100 g: 149 b: 237 range: 255);
		at: 'cornsilk' put: (Color r: 255 g: 248 b: 220 range: 255);
		at: 'crimson' put: (Color r: 220 g: 20 b: 60 range: 255);
		at: 'cyan' put: (Color r: 0 g: 255 b: 255 range: 255);
		at: 'darkblue' put: (Color r: 0 g: 0 b: 139 range: 255);
		at: 'darkcyan' put: (Color r: 0 g: 139 b: 139 range: 255);
		at: 'darkgoldenrod' put: (Color r: 184 g: 134 b: 11 range: 255);
		at: 'darkgray' put: (Color r: 169 g: 169 b: 169 range: 255);
		at: 'darkgreen' put: (Color r: 0 g: 100 b: 0 range: 255);
		at: 'darkgrey' put: (Color r: 169 g: 169 b: 169 range: 255);
		at: 'darkkhaki' put: (Color r: 189 g: 183 b: 107 range: 255);
		at: 'darkmagenta' put: (Color r: 139 g: 0 b: 139 range: 255);
		at: 'darkolivegreen' put: (Color r: 85 g: 107 b: 47 range: 255);
		at: 'darkorange' put: (Color r: 255 g: 140 b: 0 range: 255);
		at: 'darkorchid' put: (Color r: 153 g: 50 b: 204 range: 255);
		at: 'darkred' put: (Color r: 139 g: 0 b: 0 range: 255);
		at: 'darksalmon' put: (Color r: 233 g: 150 b: 122 range: 255);
		at: 'darkseagreen' put: (Color r: 143 g: 188 b: 143 range: 255);
		at: 'darkslateblue' put: (Color r: 72 g: 61 b: 139 range: 255);
		at: 'darkslategray' put: (Color r: 47 g: 79 b: 79 range: 255);
		at: 'darkslategrey' put: (Color r: 47 g: 79 b: 79 range: 255);
		at: 'darkturquoise' put: (Color r: 0 g: 206 b: 209 range: 255);
		at: 'darkviolet' put: (Color r: 148 g: 0 b: 211 range: 255);
		at: 'deeppink' put: (Color r: 255 g: 20 b: 147 range: 255);
		at: 'deepskyblue' put: (Color r: 0 g: 191 b: 255 range: 255);
		at: 'dimgray' put: (Color r: 105 g: 105 b: 105 range: 255);
		at: 'dimgrey' put: (Color r: 105 g: 105 b: 105 range: 255);
		at: 'dodgerblue' put: (Color r: 30 g: 144 b: 255 range: 255);
		at: 'firebrick' put: (Color r: 178 g: 34 b: 34 range: 255);
		at: 'floralwhite' put: (Color r: 255 g: 250 b: 240 range: 255);
		at: 'forestgreen' put: (Color r: 34 g: 139 b: 34 range: 255);
		at: 'fuchsia' put: (Color r: 255 g: 0 b: 255 range: 255);
		at: 'gainsboro' put: (Color r: 220 g: 220 b: 220 range: 255);
		at: 'ghostwhite' put: (Color r: 248 g: 248 b: 255 range: 255);
		at: 'gold' put: (Color r: 255 g: 215 b: 0 range: 255);
		at: 'goldenrod' put: (Color r: 218 g: 165 b: 32 range: 255);
		at: 'gray' put: (Color r: 128 g: 128 b: 128 range: 255);
		at: 'grey' put: (Color r: 128 g: 128 b: 128 range: 255);
		at: 'green' put: (Color r: 0 g: 128 b: 0 range: 255);
		at: 'greenyellow' put: (Color r: 173 g: 255 b: 47 range: 255);
		at: 'honeydew' put: (Color r: 240 g: 255 b: 240 range: 255);
		at: 'hotpink' put: (Color r: 255 g: 105 b: 180 range: 255);
		at: 'indianred' put: (Color r: 205 g: 92 b: 92 range: 255);
		at: 'indigo' put: (Color r: 75 g: 0 b: 130 range: 255);
		at: 'ivory' put: (Color r: 255 g: 255 b: 240 range: 255);
		at: 'khaki' put: (Color r: 240 g: 230 b: 140 range: 255);
		at: 'lavender' put: (Color r: 230 g: 230 b: 250 range: 255);
		at: 'lavenderblush' put: (Color r: 255 g: 240 b: 245 range: 255);
		at: 'lawngreen' put: (Color r: 124 g: 252 b: 0 range: 255);
		at: 'lemonchiffon' put: (Color r: 255 g: 250 b: 205 range: 255);
		at: 'lightblue' put: (Color r: 173 g: 216 b: 230 range: 255);
		at: 'lightcoral' put: (Color r: 240 g: 128 b: 128 range: 255);
		at: 'lightcyan' put: (Color r: 224 g: 255 b: 255 range: 255);
		at: 'lightgoldenrodyellow' put: (Color r: 250 g: 250 b: 210 range: 255);
		yourself! !

!Color class methodsFor: '*css2-namedcolors' stamp: 'KenD 1/28/2013 19:32'!
css2NamedColors2
	"Answer a dictionary of CSS2 compliant color names to Color objects.
	Second half!!"

	^Dictionary new
		at: 'lightgray' put: (Color r: 211 g: 211 b: 211 range: 255);
		at: 'lightgreen' put: (Color r: 144 g: 238 b: 144 range: 255);
		at: 'lightgrey' put: (Color r: 211 g: 211 b: 211 range: 255);
		at: 'lightpink' put: (Color r: 255 g: 182 b: 193 range: 255);
		at: 'lightsalmon' put: (Color r: 255 g: 160 b: 122 range: 255);
		at: 'lightseagreen' put: (Color r: 32 g: 178 b: 170 range: 255);
		at: 'lightskyblue' put: (Color r: 135 g: 206 b: 250 range: 255);
		at: 'lightslategray' put: (Color r: 119 g: 136 b: 153 range: 255);
		at: 'lightslategrey' put: (Color r: 119 g: 136 b: 153 range: 255);
		at: 'lightsteelblue' put: (Color r: 176 g: 196 b: 222 range: 255);
		at: 'lightyellow' put: (Color r: 255 g: 255 b: 224 range: 255);
		at: 'lime' put: (Color r: 0 g: 255 b: 0 range: 255);
		at: 'limegreen' put: (Color r: 50 g: 205 b: 50 range: 255);
		at: 'linen' put: (Color r: 250 g: 240 b: 230 range: 255);
		at: 'magenta' put: (Color r: 255 g: 0 b: 255 range: 255);
		at: 'maroon' put: (Color r: 128 g: 0 b: 0 range: 255);
		at: 'mediumaquamarine' put: (Color r: 102 g: 205 b: 170 range: 255);
		at: 'mediumblue' put: (Color r: 0 g: 0 b: 205 range: 255);
		at: 'mediumorchid' put: (Color r: 186 g: 85 b: 211 range: 255);
		at: 'mediumpurple' put: (Color r: 147 g: 112 b: 219 range: 255);
		at: 'mediumseagreen' put: (Color r: 60 g: 179 b: 113 range: 255);
		at: 'mediumslateblue' put: (Color r: 123 g: 104 b: 238 range: 255);
		at: 'mediumspringgreen' put: (Color r: 0 g: 250 b: 154 range: 255);
		at: 'mediumturquoise' put: (Color r: 72 g: 209 b: 204 range: 255);
		at: 'mediumvioletred' put: (Color r: 199 g: 21 b: 133 range: 255);
		at: 'midnightblue' put: (Color r: 25 g: 25 b: 112 range: 255);
		at: 'mintcream' put: (Color r: 245 g: 255 b: 250 range: 255);
		at: 'mistyrose' put: (Color r: 255 g: 228 b: 225 range: 255);
		at: 'moccasin' put: (Color r: 255 g: 228 b: 181 range: 255);
		at: 'navajowhite' put: (Color r: 255 g: 222 b: 173 range: 255);
		at: 'navy' put: (Color r: 0 g: 0 b: 128 range: 255);
		at: 'oldlace' put: (Color r: 253 g: 245 b: 230 range: 255);
		at: 'olive' put: (Color r: 128 g: 128 b: 0 range: 255);
		at: 'olivedrab' put: (Color r: 107 g: 142 b: 35 range: 255);
		at: 'orange' put: (Color r: 255 g: 165 b: 0 range: 255);
		at: 'orangered' put: (Color r: 255 g: 69 b: 0 range: 255);
		at: 'orchid' put: (Color r: 218 g: 112 b: 214 range: 255);
		at: 'palegoldenrod' put: (Color r: 238 g: 232 b: 170 range: 255);
		at: 'palegreen' put: (Color r: 152 g: 251 b: 152 range: 255);
		at: 'paleturquoise' put: (Color r: 175 g: 238 b: 238 range: 255);
		at: 'palevioletred' put: (Color r: 219 g: 112 b: 147 range: 255);
		at: 'papayawhip' put: (Color r: 255 g: 239 b: 213 range: 255);
		at: 'peachpuff' put: (Color r: 255 g: 218 b: 185 range: 255);
		at: 'peru' put: (Color r: 205 g: 133 b: 63 range: 255);
		at: 'pink' put: (Color r: 255 g: 192 b: 203 range: 255);
		at: 'plum' put: (Color r: 221 g: 160 b: 221 range: 255);
		at: 'powderblue' put: (Color r: 176 g: 224 b: 230 range: 255);
		at: 'purple' put: (Color r: 128 g: 0 b: 128 range: 255);
		at: 'red' put: (Color r: 255 g: 0 b: 0 range: 255);
		at: 'rosybrown' put: (Color r: 188 g: 143 b: 143 range: 255);
		at: 'royalblue' put: (Color r: 65 g: 105 b: 225 range: 255);
		at: 'saddlebrown' put: (Color r: 139 g: 69 b: 19 range: 255);
		at: 'salmon' put: (Color r: 250 g: 128 b: 114 range: 255);
		at: 'sandybrown' put: (Color r: 244 g: 164 b: 96 range: 255);
		at: 'seagreen' put: (Color r: 46 g: 139 b: 87 range: 255);
		at: 'seashell' put: (Color r: 255 g: 245 b: 238 range: 255);
		at: 'sienna' put: (Color r: 160 g: 82 b: 45 range: 255);
		at: 'silver' put: (Color r: 192 g: 192 b: 192 range: 255);
		at: 'skyblue' put: (Color r: 135 g: 206 b: 235 range: 255);
		at: 'slateblue' put: (Color r: 106 g: 90 b: 205 range: 255);
		at: 'slategray' put: (Color r: 112 g: 128 b: 144 range: 255);
		at: 'slategrey' put: (Color r: 112 g: 128 b: 144 range: 255);
		at: 'snow' put: (Color r: 255 g: 250 b: 250 range: 255);
		at: 'springgreen' put: (Color r: 0 g: 255 b: 127 range: 255);
		at: 'steelblue' put: (Color r: 70 g: 130 b: 180 range: 255);
		at: 'tan' put: (Color r: 210 g: 180 b: 140 range: 255);
		at: 'teal' put: (Color r: 0 g: 128 b: 128 range: 255);
		at: 'thistle' put: (Color r: 216 g: 191 b: 216 range: 255);
		at: 'tomato' put: (Color r: 255 g: 99 b: 71 range: 255);
		at: 'turquoise' put: (Color r: 64 g: 224 b: 208 range: 255);
		at: 'violet' put: (Color r: 238 g: 130 b: 238 range: 255);
		at: 'wheat' put: (Color r: 245 g: 222 b: 179 range: 255);
		at: 'white' put: (Color r: 255 g: 255 b: 255 range: 255);
		at: 'whitesmoke' put: (Color r: 245 g: 245 b: 245 range: 255);
		at: 'yellow' put: (Color r: 255 g: 255 b: 0 range: 255);
		at: 'yellowgreen' put: (Color r: 154 g: 205 b: 50 range: 255);
		yourself! !

!Color class methodsFor: '*css2-namedcolors' stamp: 'jmv 2/14/2013 09:22'!
fromCSS2String: aString
	"Answer a color from the given W3C spec.
	Color fromCSS2String: 'aliceblue'
	"

	((aString beginsWith: '#') or: [ aString beginsWith: 'rgb' ])
		ifTrue: [ ^self fromString: aString ].
	^self css2ColorDictionary at: aString ifAbsent: nil! !
