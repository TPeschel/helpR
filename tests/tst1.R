rm( list = ls( ) )

library( helpR )
library( ggplot2 )
library( arm )
library( effects )

read.all.tables(
    directory =  "~/LIFE/github-tpeschel/R/WiebkeKeller/data/",
    pattern = "*.xlsx" )

my$names
my$colums
my$tables

my$names[ 1 ]
my$tables[[ 1 ]]$name
my$colums[ 1 ]

my$tables[["D00078"]]

my$names[ 6 ]
my$tables[[ 6 ]]$name
my$colums[ 6 ]

my$names[ 7 ]
my$tables[[ 7 ]]$name
my$colums[ 7 ]
my$tables[[ 7 ]]$data[ , c( "TEILNEHMER_SIC", "TEILNEHMER_GEB_JJJJMM" ) ]

focus <-
    merge(
        x = get.table.by.name( "D00159" ),
        merge(
            x = get.table.by.name( "R00001" ),
            y = get.table.by.name( "D00078" ),
            by.x = c( "TEILNEHMER_SIC" ),
            by.y = c( "C_SOZDEM_SIC" ),
            all = F ),
        by.x = c( "WINKLER_SIC", "WINKLER_EDAT" ),
        by.y = c( "TEILNEHMER_SIC", "C_SOZDEM_EDAT" ),
        all = F )

focus <-
    merge(
        x = get.table.by.index( 6 ),
        merge(
            x = get.table.by.index( 7 ),
            y = get.table.by.index( 1 ),
            by.x = c( "TEILNEHMER_SIC" ),
            by.y = c( "C_SOZDEM_SIC" ),
            all = F ),
        by.x = c( "WINKLER_SIC", "WINKLER_EDAT" ),
        by.y = c( "TEILNEHMER_SIC", "C_SOZDEM_EDAT" ),
        all = F )


ggplot( focus ) +
    geom_boxplot( aes( as.factor( C_SOZDEM_WOHNGEG ), WINKLER_SCORE_FAM ) )

focus$C_SOZDEM_EINZELKIND <-
    as.factor( focus$C_SOZDEM_EINZELKIND )

focus$C_SOZDEM_EIGZIMM <-
    as.factor( focus$C_SOZDEM_EIGZIMM )

focus$reli <- factor(
    focus$C_SOZDEM_KE_RELIGION, c( 1 : 6 ),
    c( "konfessionslos", "katholisch", "evangelisch", "christlich", "islamisch", "anders" ) )

str( focus$reli )

lm.1 <- glm(
    data = focus,
    formula =  C_SOZDEM_EINZELKIND ~ reli * C_SOZDEM_EIGZIMM,
    family = "binomial",
    na.action = na.omit )

cdplot( C_SOZDEM_EINZELKIND ~ reli, data =  focus )
cdplot( C_SOZDEM_EINZELKIND ~ C_SOZDEM_EIGZIMM, data =  focus )

summary( lm.1 )

plot( allEffects( lm.1 ) )
plot( Effect( "C_SOZDEM_EIGZIMM", lm.1 ) )

invlogit( coef( lm.1 )[ 1 ] )
invlogit( coef( lm.1 )[ 1 ] + coef( lm.1 )[ 2 ] )
invlogit( coef( lm.1 )[ 1 ] + coef( lm.1 )[ 3 ] )
invlogit( coef( lm.1 )[ 1 ] + coef( lm.1 )[ 4 ] )
invlogit( coef( lm.1 )[ 1 ] + coef( lm.1 )[ 5 ] )
invlogit( coef( lm.1 )[ 1 ] + coef( lm.1 )[ 6 ] )

## !!! this gives not the correct confidence interval
exp( confint( lm.1 ) )

## but this does the job
invlogit( confint( lm.1 )[ 1, ] )
invlogit( confint( lm.1 )[ 1, ] + confint( lm.1 ) [ 2, ] )
invlogit( confint( lm.1 )[ 1, ] + confint( lm.1 ) [ 3, ] )
invlogit( confint( lm.1 )[ 1, ] + confint( lm.1 ) [ 4, ] )
invlogit( confint( lm.1 )[ 1, ] + confint( lm.1 ) [ 5, ] )
invlogit( confint( lm.1 )[ 1, ] + confint( lm.1 ) [ 6, ] )

