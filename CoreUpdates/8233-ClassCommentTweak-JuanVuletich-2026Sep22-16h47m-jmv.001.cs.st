'From Cuis7.9 [latest update: #8216] on 22 September 2026 at 4:48:10 pm'!

!MethodChangeRecord commentStamp: 'jmv 22/Sep/2026, 7:21:36 pm (UTC)' prior: 0!
MethodChangeRecords are used to record method changes.  Here is a simple summary of the relationship between the changeType symbol and the recording of prior state

					|	prior == nil			|	prior not nil	
	---------	|----------------------------	|--------------------
			add		|	add					|	change
	---------	|----------------------------	|--------------------
			remove	|	addedThenRemoved	|	remove

Structure:
changeType		symbol -- as summarized above
currentMethod	method
				This is the current version of the method.
				It can be used to assert this change upon entry to a layer. 
infoFromRemoval -- an array of size 2.
				The first element is the #historyPointer of the last version of the method.
				The second element is the category in which it was defined, so it
				can be put back there if re-accepted from a version browser.

Note that the above states each have an associated revoke action:
	add --> remove
	change --> change back
	remove --> add back
	addedThenRemoved --> no change
However all of these are accomplished trivially by restoring the original method dictionary.!

