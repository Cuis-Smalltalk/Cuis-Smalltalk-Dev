'From Cuis7.7 [latest update: #7953] on 20 May 2026 at 9:34:45 am'!

!RunArray methodsFor: 'inspecting' stamp: 'zrdz 20/May/2026 09:33:31'!
explorerContents
	| runStart nextRunStart explorerWrappers |

	(Preferences at: #compressRunArrayValues) ifFalse: [
		^ super explorerContents ].

	explorerWrappers := OrderedCollection new.
	explorerWrappers add: (ObjectExplorerWrapper with: runs name: 'runs' model: self).
	runStart := 1.
	runs with: values do: [ :runSize :attributes |
		nextRunStart := runStart + runSize.
		explorerWrappers add: (ObjectExplorerWrapper
			with: attributes
			name: (runStart printString, '->', (nextRunStart-1) printString)
			model: self).
		runStart := nextRunStart ].

	^ explorerWrappers! !

"Postscript: Lodge compressRunArrayValues as a preference to alter the
 behaviour of `RunArray`s when being explored."
| pref name | 
name := #compressRunArrayValues.
pref := Preference new name: name
 description: 'If set, RunArray will have a compressed view, not repeating every value at every index, but instead showing what is present at a given run.'
 category: #inspection
 type: Boolean
 value: true.
Preferences allPreferences at: name put: pref.!
