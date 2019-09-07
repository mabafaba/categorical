Categorical Vectors
-------------------

### features

Categorical Vectors are a basic vector type with the following features:

-   they have a fixed set of allowed values called 'levels' (similar to
    levels in factors)
-   they can store alternative values for each level; you can switch
    back and forth between alternatives.
-   each record in the vector can have more than one level 'selected' to
    allow storing multipe response data.
-   when printing categorical values, they are wrapped in single quotes
    (between factors that are printed with no quotes and characters that
    are printed with double quotes)

### Example: days of the week

let's store the days of the week as integer values 1-7, but include
labels in two different languages, and another alternative value that
defines whether the day is on the weekend or not:


    mydays <- categorical(c(1,2,1,5,6,3,7),
                                    levels=1:7,
                                    labels_english = c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
                                    labels_german = c('Montag','Dienstag','Mittwoch','Donnerstag','Freitag','Samstag','Sonntag'),
                                    day_type = c('week','week','week','week','week','weekend','weekend')
                                    )

we can `alternate` between the original and alternative values:


    mydays_en <- alternate(mydays, "labels_english")

You can see that the levels remain untouched. we can `alternate` back to
the original levels if we don't name the alternative:

    alternate(mydays_en)
    #> '1' '2' '1' '5' '6' '3' '7'
    #> levels: 1 2 3 4 5 6 7

let's subset the vector to only keep the weekend days:


    my_weekend_days_en <- mydays_en[alternate(mydays_en, 'day_type')==categorical('weekend')]
    my_weekend_days_en
    #> categorical vector of length 0
    #> levels: 1 2 3 4 5 6 7

`categorical` gives you four new basic vectors:

-   `categorical` itself: a vector with levels (predefined categorical
    values in the vector, just like in factors), labels (sort of like
    names), and the option to define any number of alternatives values
    for each level. The main values can be characters, numbers or
    factors. Each record / item in the vector can have any number of
    levels selected (including none).
-   three specific classes derived from it: `ordinal` and `range`

The `categorical` class
-----------------------

    library(dplyr)
    #> Warning: package 'dplyr' was built under R version 3.5.2
    #> 
    #> Attaching package: 'dplyr'
    #> The following objects are masked from 'package:stats':
    #> 
    #>     filter, lag
    #> The following objects are masked from 'package:base':
    #> 
    #>     intersect, setdiff, setequal, union

    my_colors <- categorical(x = c("red","blue" , "red","red", "blue"), 
                             levels =  c("red", "green", "blue"),
                             hexcode = c("#FF0000","#00FF00","#0000FF"))

    my_colors                
    #> 'red' 'blue' 'red' 'red' 'blue'
    #> levels: red green blue

you can switch to the alternative value at any time:

    my_colors_as_hexcodes<-alternate(my_colors, alternative = 'hexcode')
    my_colors_as_hexcodes
    #> '#FF0000' '#0000FF' '#FF0000' '#FF0000' '#0000FF'
    #> levels: red green blue

if you don't provide an `alternative` argument, the vector is reverted
to the original value:


    alternate(my_colors_as_hexcodes)
    #> 'red' 'blue' 'red' 'red' 'blue'
    #> levels: red green blue

Note that multiple 'original' values can have the same alternative; one
'original' can however not point to multiple different alternatives. So
the 'original' vector should always be the most granular one.

### `categorical` vectors with levels and alternatives


    # What is your favourite colour?

    my_colors <- categorical(x = c("red","blue" , "red","red", "blue"), 
                             levels =  c("red", "green", "blue"),
                             hexcode = c("#FF0000","#00FF00","#0000FF"),
                             temperature = c('warm', 'cool', 'cool'))


    color_temp <- alternate(my_colors, 'temperature')

    color_temp
    #> 'warm' 'cool' 'warm' 'warm' 'cool'
    #> levels: red green blue

    alternate(color_temp)
    #> 'red' 'blue' 'red' 'red' 'blue'
    #> levels: red green blue

We can switch as suitable:

    colour_df<-tibble(my_colors)
    colour_df
    #> # A tibble: 5 x 1
    #>   my_colors
    #>    <ctgrcl>
    #> 1     'red'
    #> 2    'blue'
    #> 3     'red'
    #> 4     'red'
    #> 5    'blue'

### Multiple selection

`categorical` vectors support *multiple selection*. That means that for
each element of a vector, each level can be 'selected' or not selected:


    # What are your favourite colour? (some people like more than one color, or even no colors at all!)



    many_favourite_colours <- categorical(list(c('red'),
                                               c('blue', 'orange', 'yellow'),
                                               c('yellow', 'magenta'),
                                               c('black'),
                                               c()),
                                          levels = c('red','blue','orange','yellow','magenta','black'))

    many_favourite_colours
    #>  (1) 'red'  (3) 'blue' & 'orange' & 'yellow'  (2) 'yellow' & 'magenta'  (1) 'black'  (0) ''
    #> levels: red blue orange yellow magenta black

Since we didn't specify the levels, they were set automatically to the
unique values in the supplied vector. Note that the levels correspond to
all given answers, not answer combinations:

    levels(many_favourite_colours)
    #> [1] "red"     "blue"    "orange"  "yellow"  "magenta" "black"

### Ordinal class

    # colors ordered by wavelength
    my_lightwaves<-ordinal(x = c('red','infra','violet','blue','red', 'infra'),
                    levels = c('violet','blue','red', 'infra'),rank = c(1,2,3,4))

    my_lightwaves
    #> #3-red #4-infra #1-violet #2-blue #3-red #4-infra

Ordinals as all categorical classes support multiple selection:

    my_lightwaves<-ordinal(x = list(c('red','infra'),
                                    'violet',
                                    c('blue','red'),
                                    'infra'),
                    levels = c('violet','blue','red', 'infra'),rank = c(1,2,3,4))



    my_lightwaves
    #> (#3-red & #4-infra) #1-violet (#2-blue & #3-red) #4-infra

Intervals
---------
