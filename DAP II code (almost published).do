
/* I. Quantitative outcome, quantitative predictor

	i. Univariate analysis

		This is the simplest case, though it is worth looking at this for 
		all students since it a) is the most common technique, by far, at
		higher levels, and b) some of the techniques below build on it. 

		Regression is most useful when at least the outcome variable is nicely 
		quantitative. In such a case, we can do some simple inference about 
		each  variable's population mean before looking at the relationship 
		between them.

		Let's revisit our simple, very common example from Part I. 
		Remember that  at one point, I showed you all how to regress 
		income on education. 

		Typically, a confidence interval is best for doing inference about the 
		likely population parameter's value unless we have a specific reason 
		to test some specific hypothetical value of the population. That's 
		more common in the case of our bivariate analysis, where we typically 
		report both the t-statistic and the CI . Usually a CI is sufficient 
		for the univariate analysis. 

		CI-time! */ 


		ci mean educ

		ci mean rincom16

		/* What do these folks mean? Using a procedure that captures the 
		population parameter in its bounds 95 percent of the time (in the
		long-run), we can say that the likely population parameter for 
		education -- here, the mean --  lies between 13.6 and 13.9 years 
		of education. For income, the mean lies between 15.3 and 16.0 (this 
		measure is technically not QUITE a true quant. variable, but it's 
		close enough; we can say that the mean is probably between 
		25k and 30k).
		
		If we want to, we can check our work by hand. First, we grab the sample
		statistic (I'll just show this for education since the procedure is 
		the same in both cases). */ 
		
		sum educ /* OK, we can see the mean and standard deviation there. We
		can calculate a 95 percent CI here, and since our n is so large, we 
		can basically use the 1.96 t* here, same as if we have a truly Normal
		sampling distribution. */ 
	
		disp 13.73 - 1.96*(2.97/sqrt(2345)) 
		
		disp 13.73 + 1.96*(2.97/sqrt(2345))
		
		/* OK, cool, this checks out! 
	
	ii. Bivariate analysis

		OK, let's do regression.

		(As a friendly reminder, the idea of regression can be traced all the 
		way back to two of our simplest measures of a distribution, the mean 
		and the standard deviation. Correlation is basically just the 
		covariance -- literally, the product of the two variances -- of two 
		variables, x and y, only divided by the product of their SD. 
		Regression is then the correlation multiplied by SDy/SDx, where 
		y is whatever variable you consider the outcome and x is whatever 
		variable you consider the predictor). 

		Now, we're going to see how Stata reports some useful statistics when we
		tell it to regress income on education. We haven't yet had our 
		lecture on  doing inference from regression, but the concept is 
		a very straightforward extension of what we already know about 
		inference. Let's get the output up first. */ 

		reg rincom16 educ

		/* Now, notice how, in the output, we see a t-statistic on both the
		educ coefficient and the "constant coefficient" (though technically 
		the constant is not a coefficient as it's not multiplied by anything; 
		this is a Stata quirk)? And notice how they have a p-value as well? 
		Well, it turns out that (forgive the all-caps; this is a key point) 
		REGRESSION COEFFICIENTS, JUST LIKE MEANS AND PROPORTIONS, ARE SAMPLE 
		STATISTICS THAT CAN BE CONSIDERED TO BE DRAWN FROM NORMAL-APPROXIMATING 
		SAMPLING DISTRIBUTIONS. THEREFORE, WE CAN  CONDUCT HYPOTHESIS TESTS 
		ON WHETHER A REGRESSION COEFFICIENT DIFFERS FROM SOME HYPOTHETICAL 
		VALUE, SAY, ZERO. THE T-STAT IN STATA OUTPUT SIMPLY TELLS US  THE 
		TEST-STATISTIC -- THE STANDARDIZED VALUE OF OUR ACTUAL SAMPLE 
		STATISTIC, IN THIS CASE THE REGRESSION COEFFICIENT -- FOR THE 
		NULL HYPOTHESIS THAT THE  POPULATION REGRESSION SLOPE IS ZERO. THE 
		P-VALUE CAN BE IMMEDIATELY INTERPRETED AS A TWO-TAILED HYPOTHESIS 
		TEST (to get a one-sided, just divide the p-value by two, though 
		I am officially discouraging you all from using one-sided 
		hypothesis tests -- at least report both p-values). 

		Notice also that Stata automatically reports confidence 
		intervals. I'd like  you all to report and interpret these 
		if you do regression. 

		So, let's interpret our results. If we observe from an SRS 
		of size 1362 a regression slope of 0.76 when income is regressed 
		on years of education the respondent obtains, this would virtually 
		never occur if the slope. Using a procedure which captures the 
		true population parameter in its bounds 95 percent of the time, 
		we can say that the likely range of values for the  population
		parameter is 0.65 to 0.87. 

		Easy enough? Easy enough. 

II. Categorical outcome, categorical predictor

	1. Two possible values of outcome and of predictor
	
	i. Univariate analysis, two possible values of two categorical vars. 
	
		Let's suppose here that we either do not have a dummy variable
		to begin with, or that we do have a dummy variable, but it is not
		coded as taking on values of zero and one in the GSS file. This is 
		an easy fix. Remember that a dummy variable is one which takes on
		only one of two possible values, 0 and 1, which stand for SOME KEY
		QUALITATIVE VALUE (coded as 1) and everything but that (coded as 0). 
		The variable might naturally split into two groups like that -- e.g., 
		was r alive in 1940 -- or it might be a reasonable reconfiguration 
		of the variable. 

		Let's work with the subjective class variable and relate it 
		to home ownership. This is the same example as from the most 
		recent discussion section, and I discuss some of the reason that 
		this might be an intuitive question in those slides. I omit it here 
		for concision.
		
		Let's change class into a dummy from its four-possible-value form. 
		Here, I've decided to brick the three "popular classes" -- lower, 
		working, middle -- into one group and to separate out the elite. */ 

		gen realclass = .
		replace realclass = 1 if class == 1 
		replace realclass = 1 if class == 2
		replace realclass = 1 if class == 3
		replace realclass = 0 if class == 4
		tab realclass class
		
		/* Notice that what we're doing here, in general, is assigning all
		existing values of a variable to either "1" or "0" to a new variable.
		You should assign values in a way that makes sense by grouping like
		items together: e.g., here, I have put the three "lowest" classes 
		together and separated out the highest class according to a social 
		theory that  says that many self-identified middle-class people are 
		in fact working class and many middle-class people are involved in 
		certain relationships with big capital that resembles the 
		worker-capitalist polarity.
		
		Any variable of which you are especially interested in ONE value
		can be turned into a dummy in this way; see the most recent set of 
		slides  or the first .do-file for an example of this using the 
		religion variable and the new dummy variable "protestant". */ 

		/* Let's get those labels looking nice and clean. I recommend doing 
		this both for aesthetic reasons and so that you don't have to keep 
		the meaning of 0 and 1 in your head the whole time. [I used to get
		griped at for not doing this. Learn from my mistake :) ].*/

		label define classkey 0 "elite" 1 "popular classes"
		label values realclass classkey /* groovy. 
		
		Time to look at the home ownership variable.*/ 
		
		codebook dwelown

		/* OK, now notice that although our outcome is almost a
		binary variable, it's not in "classical" dummy format even if
		we simply drop the very small "other" category -- this is because the
		numerical values for the categorical outcomes are not
		0 and 1, and therefore Stata gets grumpy with us.

		You can try this code just to see what happens, though I'm going 
		to leave it out because errors in your .do-file stop it from 
		running. BAD CODE IN CASE YOU WANT TO SEE WHAT HAPPENS OR IN CASE YOU
		GET THIS ERROR YOURSELF AND ARE CONFUSED: prtest colcom, by(realclass) 
		
		Simple fix: recode the variable. Here, we're sort of just ignoring
		the possible value "other", which is unfortuante but probably the
		least of all evils here since there is no making head or tail of 
		it. */ 

		gen homeowner = . 
		replace homeowner = 1 if dwelown == 1
		replace homeowner = 0 if dwelown == 2
		label define homeowner2 0 "renter" 1 "owner"
		label values homeowner homeowner2

		tab homeowner dwelown /* Remember that this is just a check to make 
		sure that we did our re-code correctly. It is NOT a real two-way
		table because it is not two different variables, but one variable 
		re-coded. Use this ONLY to check you work. 
		
		Now, let's get the tests for each proportion going. */ 
		
		ci proportion realclass
		
		ci proportion homeowner
		
		/* How to interpret this? Using a procedure that captures the 
		population parameter in its bounds 95 percent of the time (in the
		long-run), we can say that the likely population parameter for 
		class -- here, the proportion that is part of the self-identified
		popular classes -- lies between 95.6 and 97.2 percent of the 
		population. 
		
		What about home ownership? Using a procedure ... the proportion of 
		individuals who own their own home ranges between 59 percent and 64
		percent. 
		
	ii. bivariate analysis, two possible values of two categorical vars.

		Now, let's do our bivariate analysis. I'm going to show how to do this
		by hand first, which is OPTIONAL. It's just because we CAN do this by
		hand, but we will not go through the severe tedium of regression by
		hand, that I show this and not regression by hand. But, by all means, 
		include a hand calculation if you like. */ 
		
		tab realclass homeowner
		
		/* This is a true two-way table that shows us how two categorical 
		variables relate. Let's calculate a a test of a difference in
		proportions. 
		
		First, we solve by hand. Again, this is optional*/ 

		disp (44/50)-(891/1470) /* Write down the raw diff. in prop. */ 
		disp (935/1520) /* Write down the pooled proportion */
		disp 1 - .61513158 /* Write down 1 - pooled */ 
		disp (1/50 + 1/1470) /* Write down 1/n1 + 1/n2 */ 
		disp (.27387755)/sqrt((.61513158*.38486842)*(.02068027)) /* Calculate 

		Here's the Stata code for that same test*/ 
		
		prtest homeowner, by(realclass) 
		
		/* Results check out! Let's interpret the results: we would see a 
		difference in sample proportions this extreme or more sheerly by 
		chance only 0.01 percent  of the time if there really were no 
		difference in the population proportions. 
	
	2. >2 possible outcomes of categorical predictor and/or outcome 
	
	i. Univariate analysis for categorical variable with more than two
	possible values of either predictor, outcome, or both.
	
		Let's keep this part concise. How about we go back to our old friend,
		the religion variable, which is a prime example of a polytomous 
		variable with many categories and no possible ranking of them variables 
		even if we give them numerical values just to keep track of them. 
		I am NOT using my dummy protestant variable here precisely because 
		the chi-square test is useful in cases where we DON'T want to reduce
		a polytomous variable to a dummy variable.
		
		What if we want to test the independence assumption of two such 
		variables, say religion and our ORIGINAL class variable, which, 
		you should recall, is polytomous (our recoded variable "realclass" 
		turns it into a dummy). */ 
		
		/* Let's first get a rough sense of what the population distribution 
		is like for each variable. It's kind of hard to do anything like a
		CI here, so you can do a goodness-of-fit test with some hypothetical
		population proportions for each variable if you like. First, we 
		need to install the goodfness of fit command. */ 
		
		
		search csgof /* Now, go ahead and install it */ 
		
		/* Now we can tell Stata to test whether or not the values of our
		variable have a particular probability distribution. Here's the generic
		code: 
		
		csgof VARIABLE, expperc(PROPORTION-OF-VAR-CODED-AS-1 [space]
		PROPORTION-OF-VAR-CODED-AS-2 ... etc.)*/ 
		
		csgof class, expperc(25 25 25 25) /* As the careful observer might
		notice through her investigation of American society, we don't live 
		in a  society where class is a finely-graded ladder with people 
		spread out more or less evenly--those of you who have taken my 
		"substantial  sociology" courses will remember us spending 
		quite a bit of time on class and  this (incorrect) null hypothesis. 
		
		We could do the same thing for religion but in this case, there
		are so many possible expected percents that it might seem kind of
		pointless. As we are about to see, however, this large number of 
		possible values will pose some problems for our chi-square test, 
		which is not infinitely flexible. In general, since there isn't 
		really an equivalent of a CI for a CSGOF test use a judgment call
		if you want to do a chi-square goodness-of-fit test for the 
		univariate analysis.: how many  possible values of the variable 
		are there? Is my goodness-of-fit null a reasonable hypothesis or 
		just a bunch of random guesses? 
		
		Let's do a preliminary check to see if we can do the chi-square
		on these two variables without modifying either. We'll use the 
		"expected" option on the tab[ulate] command to see if our
		expected counts meet our two rules: no more than 1/5 of the cells
		should be <5 and none should be less than 1. */ 
		
		tab relig class, expected 
	
		/* Rats! We have way too many cells with an expected count below 
		five and several cells with expected counts below zero. Let's restrict
		our test to the largest denominations by number */ 
	
		gen bigrelig = .
		replace bigrelig = 1 if relig ==1
		replace bigrelig = 2 if relig ==2
		replace bigrelig = 3 if relig ==3
		replace bigrelig = 4 if relig ==4
		
		label define bigrelig1 1 "Protestant" 2 "Catholic" 3 "Jewish" 4 "none"
		label values bigrelig bigrelig1 /* This only uses the four biggest
		categories; it sucks to have to leave out other groups, but the math
		becomes unreliable if we have too many groups that have very small
		total counts */ 		
		
		tab bigrelig class, expected /* OK, our expected count rules look 
		fulfilled. Now let's go ahead and try our goodness-of-fit stats
		on this new variable. Here, I'm just using some rough guesses: 
		maybe Protestant folk are 50 percent, Catholic folk 30, Jewish 
		folk 5 and a-religious folk 15 percent of the big religions? This 
		is not a scientific process, just a rough guess. */
		
		csgof bigrelig, expperc(50 30 5 15)
		
		/* OK, so, we can say that there is basically no evidence for 
		my loose expectations written up above. In particular, Protestants
		and a-religious folk are significantly larger than our guess. 
		
	i. Bivariate analysis for categorical variable with more than two
	possible values of either predictor, outcome, or both. 
	
		Here's the code for a chi-square test in Stata */ 
			
		tab bigrelig class, chi2
		
		/* OK, clearly these results are significant. And, we can use a 
		hand-analysis to see why. We can go back to our expected counts and 
		look at the areas of the largest divergence... */ 
		
		tab bigrelig class, expected
		
		/* (What's driving the non-independence of categories? More lower-class
		Protestants than expected. More working-class a-religious people than 
		expected. More working-class and elite Catholics than expected) 
		
		... or we can have Stata do it for us */ 
		
		tab bigrelig class, cchi2
		
		/* This command actually calculates each cell's chi-square statistic to 
		show us how much each cell contributes (notice that underneath the table 
		total is the sum of the standard scores of each cell, which is the same 
		thing we get as our total chi-square stat. in the simple command shown 
		above). This is a useful way to do some further investigation of the 
		chi-square output and I would like for those of you conducting this 
		test to do this. */ 
	
/* III. Categorical outcome, quant. predictor (only two possible values of
outcome; the one case we're just not going to learn for now is >2 possible
values of a categorical outcome; recode your outcome variable into a dummy).

	i. Univariate analysis.

		This is a simpler one. Doing this with a polytomous (many-category 
		qualitative variable) outcome is very hard; doing it with a dummy 
		(two-category qual. var.) is easy. We're going to take the easy way out
		and convert any polytomous outcomes, if your predictor is quant., to 
		dummies. 
		
		Let's suppose we want to know if income predicts whether you are married.
		This is a topic that fascinates demographers. Marriage is a classic
		polytomous category: there are NATURALLY more than two categories and
		they cannot be ranked. Widowed is not larger or smaller than unmarried. 
		
		Let's suppose that we are willing to group together the widowed, separated,
		and the divorced with the married to get a dummy variable for whether
		someone has EVER been married. */ 
		
		gen evermarried = . 
		replace evermarried = 1 if marital ==1
		replace evermarried = 1 if marital ==2
		replace evermarried = 1 if marital ==3
		replace evermarried = 1 if marital ==4
		replace evermarried = 0 if marital ==5
		
		/* First we need to do our univariate analysis. Using a CI for a mean
		makes more sense in the predictor's case, and it in turn makes more
		sense to use a CI for a proportion in the case of the outcome. */ 
		
		ci proportion evermarried
		
		ci mean rincom16
		
		/* How do we interpret these? Using a procedure that captures the 
		population parameter in its bounds 95 percent of the time (in the
		long-run), we can say that the likely population parameter for 
		ever married -- here, the proportion -- lies between 69.6 and 73.3
		percent. (The exact same CI for income is intepreted above).  
	
	ii. Bivariate analysis. 
	
		Here, we can just do a regression. */ 
		
		reg evermarried rincom16
		
		/* OK, cool! We definitely see a relationship here. Notice that we can 
		interpret the t-statistic just as before: we have a test statistic (and
		remember that correlation is basically just an arithmetic manipulation
		of the mean, the deviations, and the SD) that comes from a sampling 
		distribution, and the t-test tells us the probability of observing
		a coefficient this large or larger purely by chance if the coefficient
		really is zero (which is our default null hypothesis).
		
		The difference in this case is that we interpret our outome as the
		probability of marriage (a more sophisticated version of doing a 
		regression on binary or dummy variable is called logit, and it's a lot
		of fun mathematically. But we're going to stick with this "linear
		probability model" for 360). Don't do this with categorical variables
		with more than two outcomes; that's called multinomial logit and
		is beyond the bounds of our course. Convert your outcome variable to
		a dummy if you want to do this. 
		
		Generally, you can interpret the regression coefficient as "a 
		unit increase in the predictor leads to a y-percent increase in 
		the probability of the outcome". 
		
		You should also interpret the CI output that's automatically included 
		here; see part I above for information on how to do that. 
	
IV. Categorical predictor, quant. outcome 

	1. Only two values of the predictor
		
		i. Univariate analysis: means and proportions
		
		Here, we can use our same techniques as above with no modification. */ 
		
		ci mean rincom16
		
		ci proportion realclass
		
		/* ii. Bivariate analysis: regression on a dummy variable. 
		
		This is relatively simple: we just regress our outcome on a variable
		that is naturally in dummy form or can be reasonably converted to such. 
		I show above in several places how to convert a naturally polytomous
		variable (one that takes on multiple qualitative values) to a dummy; 
		head to the top of the file and CTRL+f for "dummy", then read what
		you find :). Here's the regression (general interpretation of 
		regression is covered above which is, as I noted elsewhere, something 
		that everyone should watch the short video for.
		
		BTW, the theory we're testing here might seem trivial, but there are
		actually some interesting reasons why class and income might only be
		loosely correlated. Here, we investigate whether this holds up. */
		
		reg rincom16 realclass
		
		/* How to interpret the t-statistic? the p-value? The same as
		always! See Part I above for lots of detail.
		
		Notice again (discussed above in part II) that this basically is 
		the same  thing as a t-test of the difference in two group means 
		because there is so little variation in the predictor. */
		
		ttest rincom16, by(realclass)
		
		/* E.g., notice that in the above, the t-stat is the same and the 
		difference in means in the t-test is the same as the regression slope.
		
		proof:*/ disp 15.46 - 20.55 /* These values are from the t-test output
		and can be compared to the regression output just above.   
	
	2. Multiple nominal values of categorical predictor
	
		i. Univariate analysis
		
		The predictor here is another case (as with the chi-square test above) 
		where it's a bit tricky to do CIs. I would recommend, if it makes sense, 
		conducting a chi-square goodness of fit test on the predictor and a 
		CI or t-test on the outcome. Let's try to do that with our predictor
		variable here, which is a question that ask the respondent about
		their opinion on the adequacy of current criminal justice penalties,
		in particular whether they are harsh enough. */ 
		
		codebook courts
		
		csgof courts, expperc(33 33 33) /* OK, this looks fairly different to
		our prediction and has a p-value of zero. Super informative? No, not 
		really. But, in lieu of learning some really complicated techniques, 
		this is at least something. 
		
		We can use our regular CI for the mean of our outcome. Let's
		pick a fun one just to change things up */
		
		ci mean numpets /* Guidelines for interpretation are discussed
		exhaustively above  
		
		ii. Bivariate analysis
		
		This is more complicated. There's a way to do this with dummy 
		variables and regression but this would require a full treatment 
		of dummy variables,  which really belongs in 361. I've just 
		introduced the basic idea here.
		
		A simpler method is to either convert your predictor to a dummy 
		variable, which I've shown several times in this .do-file and the 
		previous, or to use ANOVA. ANOVA is in our textbook, though not in 
		our syllabus. It's actually quite easy to use, so I show it here. 
		
		The basic idea of ANOVA is that we compare the variance of the 
		individual group means from the total group mean (numerator) to a 
		kind of weighted average of the variance within each group (which 
		represents the total variance of all individuals). 
		
		Really, we don't need to get too much further into the details. 
		Like all  of our test statistics, this calculation results in some 
		number, which represents a standardized score that can be placed in
		the sampling distribution under the null. Then, we simply decide: is 
		it weird enough? ANOVA test statistics come from the F-distribution 
		and have a complicated degrees of freedom situation, so just let 
		Stata compute these.*/ 
		
		anova numpets courts
		
		/* What this does is test whether or not all the group means are equal.
		Focus here especially on the p-value, which has the same interpretation 
		that it always does, conveniently: is it below a conventional 
		cutoff, say 0.05? Note that ANOVA is in one sense "multi-sided" 
		because any difference in group means triggers statistical significance. 
		But, in another sense, it is ONLY one-sided, in that the test statistic
		-- much like the chi-square --  can only be "big" in one direction 
		(all test statistic values are positive).
		
		How to interpret this? Simple: there is evidence for a group difference
		in means among the three major opinions on the adequacy of criminal
		justice's harshness where the outcome is number of pets.
		
		We can finally do a little bit of post-analysis estimation of confidence
		intervals of the difference in means. */
		
		pwmean numpets, over(courts) mcompare(tukey) effects

		/* How to interpret this? This calculates the CI at 95 percent 
		confidence for all (unique) pairwise differences between different 
		values of the group variable (notice that in general this 
		is n*(n-1)]/2, which is a fact we get from the mathematics of 
		counting). The difference is between the first and second group 
		listed [(which you can verify using the command  ...  mean yvar,
		over(xvar)].
		
		Which t-statistics are big and which p-values are small, according to 
		our traditional alpha (0.05)? Which of the pairwise comparisons 
		do not, using a 95 CI (reported here by default), include zero? 
		Those are the ones that drive the ANOVA statistic to be significant.
		
		Looks like, somewhat surprisingly, people who own lots of pets are
		generally not quite as indulgent when it comes to fellow human beings.
		That is the only significant p-value, the only CI which does not include
		a parameter of zero. 
		
		That said, while I have not, throughout this file, been having us 
		check for outliers  since that's not the main focus here and since 
		most of you have already done that process. But, what if we check 
		for outliers since I happen to know that this variable has a rather
		noticeable case of them? */ 
		
		graph box numpets
		
		/* OK so the outlier situation is kind of severe. We might want to 
		consider just people who own fewer than six, say. */ 
		
		anova numpets courts if numpets <6
		
		/* As suspected, this seems to be driven by strong opinions in mostly
		the same direction among owners of large numbers of pets, which
		we can visualize with the graph bar command and a conditional 
		operator. Now, we find no significant evidence of a difference 
		in group means*/ 
		
		graph bar (mean) numpets if numpets >6, over(courts) 
		
		/* We can also use the graph bar command to visualize this overall and 
		using just owners of a relatively small number of pets. 
		When we just look at people with non-outlier numbers of
		pets, we see relatively little relationship */ 
		
		graph bar (mean) numpets, over(courts)
		
		graph bar (mean) numpets if numpets <6, over(courts)
	
