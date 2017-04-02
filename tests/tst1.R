rm( list = ls( ) )

library( helpR )
library( ggplot2 )
library( arm )
library( effects )

all.xlsx.tables <-
    read.all.xlsx.tables( directory =  "~/LIFE/R/WiebkeKeller/data/" )

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


r00001 <-
    get.tbl( tbl.name = "R00001", all.tables =  all.xlsx.tables )

names( r00001 )

d00078 <-
    get.tbl( tbl.name = "D00078", all.tables =  all.xlsx.tables )

names( d00078 )

d00159 <-
    get.tbl( tbl.name = "D00159", all.tables =  all.xlsx.tables )

names( d00159 )

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
