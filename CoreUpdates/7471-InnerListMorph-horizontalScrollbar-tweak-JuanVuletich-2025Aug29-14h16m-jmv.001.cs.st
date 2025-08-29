'From Cuis7.5 [latest update: #7470] on 29 August 2025 at 2:16:41 pm'!

!InnerListMorph methodsFor: 'geometry' stamp: 'jmv 8/29/2025 14:14:35'!
desiredWidth
"Ok, this is a bit messed up. We need to return the width of the widest item in the list. If we grab every item in the list, it defeats the purpose of LazyListMorph. If we don't, then we don't know the size. This is a compromise -- if the list is less then 30 items, we grab them all. If not, we grab currently visible ones, until we've checked itemsToCheck of them, then take the max width out of that 'sampling', then enlarge it a bit. If you know a better way, please chime in."

	| maxW count itemsToCheck |

	itemsToCheck := 200.
	maxW := owner viewableWidth.
	count := 0.
	listItems do: [ :each |
		each ifNotNil: [
			maxW := maxW max: (self widthToDisplayItem: each contents)]].
	(count < itemsToCheck) ifTrue: [
		1 to: listItems size do: [:i | 
			(listItems at: i) ifNil: [ | item | 
				item := self item: i.
				maxW := maxW max: (self widthToDisplayItem: item contents) ].
			((count := count + 1) > itemsToCheck) ifTrue: [
				^maxW * 13//10  ]]].
	^maxW! !

