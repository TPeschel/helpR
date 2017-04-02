rm( list = ls( ) )

library( helpR )
library( ggplot2 )
library( arm )
library( effects )

a.t <-
    read.all.xlsx.tables( "~/LIFE/R/WiebkeKeller/data/" )

a.n <-
    all.xlsx.tables.names( all.tables = a.t )

a.c.n <-
    all.xlsx.tables.row.names( all.tables = a.t )

a.c.n

a.c.n[ 1 ]
a.n[ 1 ]

a.c.n[ 6 ]
a.n[ 6 ]

a.c.n[ 7 ]
a.n[ 7 ]


r00001 <-
    get.tbl( tbl.name = "R00001", all.tables =  a.t )

names( r00001 )

d00078 <-
    get.tbl( tbl.name = "D00078", all.tables =  a.t )

names( d00078 )

d00159 <-
    get.tbl( tbl.name = "D00159", all.tables =  a.t )

names( d00159 )

focus <-
    merge( 
        x = get.tbl( tbl.name = "D00159", all.tables = a.t ),
        merge( 
            x = get.tbl( tbl.name = "R00001", all.tables =  a.t ),
            y = get.tbl( tbl.name = "D00078", all.tables =  a.t ),
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

lm.1 <- glm( 
    data = focus, 
    formula =  C_SOZDEM_EINZELKIND ~ C_SOZDEM_EINKOMMEN,
    family = "binomial",
    na.action = na.omit )

summary( lm.1 )

invlogit( coef( lm.1 )[ 1 ] )
invlogit( coef( lm.1 )[ 1 ] + coef( lm.1 )[ 2 ] )
invlogit( coef( lm.1 )[ 1 ] + 12 * coef( lm.1 )[ 2 ] )

focus$C_SOZDEM_EINKOMMEN

lm.1$coefficients
ggplot( focus ) +
    geom_boxplot( aes( C_SOZDEM_EINZELKIND, C_SOZDEM_EINKOMMEN ) )

plot( Effect( "C_SOZDEM_EINKOMMEN", lm.1 ))
