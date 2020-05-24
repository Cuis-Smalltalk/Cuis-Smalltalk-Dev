'From Cuis 5.0 [latest update: #4166] on 24 May 2020 at 9:48:05 am'!

!Integer commentStamp: '<historical>' prior: 0!
I am a common abstract superclass for all Integer implementations. My implementation subclasses are SmallInteger, LargePositiveInteger, and LargeNegativeInteger.

There are three implementations of division with remainder. For 'a' and 'b' Integers:
	- Floored division, with the quotient rounded towards negative infinity: // and \\ answer 'q' and 'r' such that 'a = bq + r' with 'r abs < b abs', where 'r' is 0 or has the same sign as 'b';
	- Truncated division, with the quotient rounded towards zero: #quo: and #rem: answer 'q' and 'r' such that 'a = bq + r' with 'r abs < b abs', where 'r' is 0 or has the same sign as 'a';
	- Euclidean division with non-negative remainder: #div: and #mod: answer 'q' and 'r' such that 'a = bq + r' with '0 <= r < b abs'.

Note that, strictly speaking, all of these divisions satisfy the definition of Euclidean division. The requirements imposed on the sign of the remainder (different for each type of division) guarantee a unique choice of quotient and remainder.

Additionally, the division in the rational field is implemented with the message / that answers a Fraction 'a/b' if the result is not a whole integer.!

