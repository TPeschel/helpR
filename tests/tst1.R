rm( list = ls( ) )

library( helpR )
library( ggplot2 )
library( arm )
library( effects )

all.xlsx.tables <-
    read.all.xlsx.tables( directory =  "~/LIFE/github-tpeschel/R/WiebkeKeller/data/" )

all.names <-
    all.xlsx.tables.names( all.tables = all.xlsx.tables )

all.col.names <-
    all.xlsx.tables.row.names( all.tables = all.xlsx.tables )

all.col.names

all.col.names[ 1 ]
all.names[ 1 ]

all.col.names[ 6 ]
all.names[ 6 ]

all.col.names[ 7 ]
all.names[ 7 ]


focus <-
    merge(
        x = get.tbl( tbl.name = "D00159", all.tables = all.xlsx.tables ),
        merge(
            x = get.tbl( tbl.name = "R00001", all.tables =  all.xlsx.tables ),
            y = get.tbl( tbl.name = "D00078", all.tables =  all.xlsx.tables ),
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

focus$reli <- factor( focus$C_SOZDEM_KE_RELIGION, c( 1 : 6 ), c( "konfessionslos", "katholisch", "evangelisch", "christlich", "islamisch", "anders" ) )

str(focus$reli)


lm.1 <- glm(
    data = focus,
    formula =  C_SOZDEM_EINZELKIND ~ reli * C_SOZDEM_EIGZIMM,
    family = "binomial",
    na.action = na.omit )

summary( lm.1 )

invlogit( coef( lm.1 )[ 1 ] )
invlogit( coef( lm.1 )[ 1 ] + coef( lm.1 )[ 2 ] )
invlogit( coef( lm.1 )[ 1 ] + 6 * coef( lm.1 )[ 2 ] )

lm.1$coefficients

plot( Effect( "reli", lm.1 ) )
plot( Effect( "C_SOZDEM_EIGZIMM", lm.1 ) )

exp( coef( lm.1 ) ) / ( 1 + exp( coef( lm.1 ) ) )

exp( confint( lm.1 ) ) / ( 1 + exp( confint( lm.1 ) ) )

