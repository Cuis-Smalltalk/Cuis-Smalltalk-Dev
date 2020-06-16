'From Cuis 5.0 [latest update: #4219] on 15 June 2020 at 1:43:33 pm'!

!WeakMessageSend commentStamp: '<historical>' prior: 0!
Instances of WeakMessageSend encapsulate message sends to objects, like MessageSend. Unlike MessageSend it is not necessarily a valid mesage.  A request to value only results in a send if in fact it is valid. 

See MessageSendComments also. WeakMessageSend is used primarily for event regristration. 

Unlike MessageSend WeakMessageSend stores the receiver (object receiving the message send) as the first and only element of its array as opposed to a named ivar.
But like MessageSend, it does have
 selector		Symbol -- message selector
 arguments		Array -- bound arguments
and it also has
 shouldBeNil	Boolean --  used to ensure array of arguments is not all nils!


!Float commentStamp: '<historical>' prior: 0!
A note About Floating Point numbers and Floating Point Arithmetic.

The following is not specific to Cuis or Smalltalk at all. This is about the properties of Float numbers in any computer implementation.

If you haven't done so already, read https://en.wikipedia.org/wiki/Floating-point_arithmetic

But if you find the Wikipedia article too detailed, or hard to read, then try http://fabiensanglard.net/floating_point_visually_explained/ (get past "How Floating Point are usually explained" and read "A different way to explain...").

Other great reads are:
	"Why don't my numbers add up?":
		http://floating-point-gui.de/
and
	"What Every Computer Scientist Should Know About Floating-Point Arithmetic":
		http://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html
and also maybe
	"Comparing floating point numbers"
		https://randomascii.wordpress.com/2012/02/25/comparing-floating-point-numbers-2012-edition/

Now that you read them, and we are on the same boat, some further comments (from jmv):

Floats are (conceptually) approximate real numbers. That's why trig and other trascendental functions always answer Floats. That's why it is ok to round the result of operations. That's why Float is considered more general than Fraction in ST-80 and most Smalltalks. So, when we have a Float value, we must not think about it as a Rational but as a Real (actually as some unknown Real that could hopefully be close to the Rational we can actually represent). Keep this in mind when dealing with Floats, and especially avoid comparing them for equality.

When doing mixed operations with Floats and Fractions, Cuis, as most other Smalltalks, converts all values to Floats. Some other systems, including Pharo Smalltalk, Scheme and Lisp have two rules: when the answer is a Number, they convert to Float. But when the answer is a boolean (#<, #=, #<=, etc.) they convert to Fraction. We think this is a mistake. There should never be implicit conversions from Float to Fraction. Fractions are to hold exact values, and people expect Fractions to be exact. On the other hand, Floats are to hold approximations (and people should be aware of that!!). But an implicit conversion from Float to Fraction would give a Fraction that should not be considered an exact value (the value comes from an inexact Float), but that knowledge is lost, as it is an instance of Fraction.

If you want exact arithmetic, usual mathematical properties (like transitivity of equality), can live in the limited world of Rational numbers, and can afford a slight performance penalty, use Fraction instead. Avoid trascendental functions and never convert to Float.

In any case, most numeric computation is done on Float numbers. There are good reasons for that. One is that in most cases we don't need an exact answer. And in many cases we can't really have it: the inputs to algorithms already have a limited precision, or they use transcendental functions. And even when exact arithmetic is possible, if we are doing sound synthesis, 24 bits of resolution is enough. For image processing and graphics, the result is never more than 16 bits per channel. So, these fields don't really need 64 bit Doubles. 32 bit Floats are enough. Other fields do need 64 bit Doubles, like physics simulations and geometry. Games usually prefer special, faster 32 bit Float operations in GPUs that have greater errors but are faster.

There are some things that can be done to increase the confidence you can have on Float results. One is to do an error propagation analysis on the code you are running. This is not easy, but it is done for any widely used numerical method. Then, you can know real bounds and/or estimates of the errors made. So, understanding your inputs and your algorithms (for example error propagation, condition number, numeric stability), and using Float number if appropriate, is the usual advice.

Perhaps you have heard about "interval arithmetic". It is a bit better than simple Float, but doesn't really fix the problems.

The ultimate solution is to do Monte Carlo analysis, with random perturbation of inputs. After the Monte Carlo run, it is needed to do statistical analysis of possible correlations between the distributions of the random noise added to imputs and the result of the algorithm.

Additional food for thought: http://www.cs.berkeley.edu/~wkahan/Mindless.pdf . According to this, doing Monte Carlo as described above attacks a slightly different problem. This might be yet another reason (besides performance) to try something like the next paragraph. I (jmv) came up with it, and I don't really know if it has been described and or tried before or not. Mhhh. Maybe a defensive publication is in order.

A possibility that could be a practical solution, being much cheaper than Monte Carlo, but better than interval arithmetic, is to represent each value by 2 Floats: an estimation of the real value (i.e. an estimation of the mean value of the distribution of the corresponding Monte Carlo result), and an estimation of the error (i.e. an estimation of the standard deviation of the corresponding Monte Carlo result). Or perhaps even 3 of them. In addition to the estimation of the real value and an estimation of the error, we could add a hard bound on the error. In many cases it will be useless, because the error can not really be bound. But in those cases where it is possible to bound it, applications could really know about the quality of computed values.

=======================================================================

My instances represent IEEE 754 floating-point double-precision numbers. They have about 16 decimal digits of accuracy and their range is between plus and minus 10^307. Some valid examples are:
	
	8.0 13.3 0.3 2.5e6 1.27e-30 1.27e-31 -12.987654e12

Mainly: no embedded blanks, little e for tens power, and a digit on both sides of the decimal point. It is actually possible to specify a radix for Float constants.  This is great for teaching about numbers, but may be confusing to the average reader:

	3r20.2 --> 6.66666666666667
	8r20.2 --> 16.25

If you don't have access to the definition of IEEE754, you can figure out what is going on by printing various simple values in Float hex.  It may help you to know that the basic format is...
	sign		1 bit
	exponent	11 bits with bias of 1023 (16r3FF), substracted to produce an actual exponent in the range -1022 .. +1023
				- 16r000:
					significand = 0: Float zero
					significand ~= 0: Denormal number (actual exponent is -1022, not -1023. No implicit leading '1' bit in mantissa)
				- 16r7FF:
					significand = 0: Infinity
					significand ~= 0: Not A Number (NaN) representation
	mantissa	53 bits, but only 52 are stored (20 in the first word, 32 in the second).  This is because a normalized mantissa, by definition, has a 1 to the right of its floating point, and IEEE 754 omits this redundant bit to gain an extra bit of precision instead.  People talk about the mantissa without its leading one as the FRACTION, and with its leading 1 as the SIGNFICAND.

The single-precision format is...
	sign		1 bit
	exponent	8 bits with bias of 127 (16r7F, substracted to produce an actual exponent in the range -126 .. +127
				- 16r00:
					significand = 0: Float zero
					significand ~= 0: Denormal number (actual exponent is -126, not -127. No implicit leading '1' bit in mantissa)
				- 16rFF:
					significand = 0: Infinity
					significand ~= 0: Not A Number (NaN) representation
	mantissa	24 bits, but only 23 are stored
This format is used in FloatArray (qv), and much can be learned from the conversion routines, Float asIEEE32BitWord, and Float class fromIEEE32Bit:.

You might also check https://en.wikipedia.org/wiki/IEEE_754_revision

Other great reads (covering broader but interesting issues):
https://randomascii.wordpress.com/2013/07/16/floating-point-determinism/
https://web.archive.org/web/20150909015114/http://www.ima.umn.edu/2010-2011/W1.10-14.11/activities/Leeser-Miriam/Leeser-GPU-IMA-Jan2011.pdf
!


!SmallInteger commentStamp: '<historical>' prior: 0!
In 32-bit images my instances are 31-bit numbers, stored in twos complement form. The allowable range is approximately +- 1 billion.

In 64-bit images my instances are 61-bit numbers, stored in twos complement form. The allowable range is approximately +- 10^18 (+- 1 quintillion)

(See SmallInteger minVal, maxVal).

Of the various classes in the Number hierarchy, SmallInteger gives:
- Maximum performance
- Top precision
- Restricted possible values

LargePositive(Negative)Integer and Fraction give increasing generality (more possible values) at the expense of performance.

Float gives more generality at the expense of precision.

Please see the class comments of the other Number classes.!

