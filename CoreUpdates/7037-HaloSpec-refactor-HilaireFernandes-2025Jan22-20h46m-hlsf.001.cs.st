'From Cuis7.3 [latest update: #6992] on 22 January 2025 at 8:57:01 pm'!

!HaloSpec class methodsFor: 'instance creation' stamp: 'hlsf 1/22/2025 20:50:06'!
from: array
" See iconicHaloSpecifications "
	| color |
	color := Color.
	array fourth do: [ :sel | color := color perform: sel].
	^ HaloSpec new 
		horizontalPlacement: array second
		verticalPlacement: array third 
		color: color
		iconSymbol: array fifth
		addHandleSelector: array first
		hoverHelp: array sixth! !


!PreferenceSet class methodsFor: 'sys preference' stamp: 'hlsf 1/22/2025 20:54:58'!
installHaloPreferencesWith: anArray
	^ self sysPreferences 
		name: #haloSpecifications 
		description: 'Halo specifications describing which halos are to be used, what they should look like, and where they should be situated'
		category: #gui
		type: Array
		value: (anArray collect: [ :each | HaloSpec from: each ])! !


!HaloSpec class reorganize!
('instance creation' from:)!

