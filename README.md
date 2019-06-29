# HamsterSTER-Analysis
A analysis of a social network of hamsters conducted in R and Gephi.
![](https://github.com/michaelFarrier/HamsterSTER-Analysis/blob/master/resources/hamster-network.png)

# Contributors

Advisor: Dr. Michael Tsikerdekis  
William Clem  
Michael Farrier  
Jade Moksness  
Cody Pranger  

# Description
HamsterSTER is a social network that took inspiration from friendSTER. Instead of friends, each user is a hamster. Hamsters can befriend other hamsters. For this project we proposed that hamster accounts added to the website on the same day and same location are most likely from the same hamster owner. To test this, we used ERGM models presented in R that presents a probability we can use to confirm or deny the given proposal. We also wanted to test the relationship of cliques and locations in the social network. Gephi was used to visualize the social network and R was used to visualize the clique behavior. For more information, see our offical poster below!

# Useful links
[Offical Poster](https://github.com/michaelFarrier/HamsterSTER-Analysis/blob/master/resources/hamster-poster.pdf)  
[Legend](https://github.com/michaelFarrier/HamsterSTER-Analysis/blob/master/resources/network-legend.png)
# Works Cited

@MISC{konect:2016:petster-friendships-hamster,
    title = {Hamsterster friendships network dataset -- {KONECT}},
    month = sep,
    year = {2016},
    url = {http://konect.uni-koblenz.de/networks/petster-friendships-hamster}
}


@inproceedings{konect,
	title = {{KONECT} -- {The} {Koblenz} {Network} {Collection}},
	author = {Jérôme Kunegis},
	year = {2013},
	booktitle = {Proc. Int. Conf. on World Wide Web Companion},
	pages = {1343--1350},
	url = {http://userpages.uni-koblenz.de/~kunegis/paper/kunegis-koblenz-network-collection.pdf}, 
	url_presentation = {http://userpages.uni-koblenz.de/~kunegis/paper/kunegis-koblenz-network-collection.presentation.pdf},
}
