#' get.tbl
#'
#' @param name name of table eg. D00100
#'
#' @return
#' @export
#'
#' @examples get.tbl( "D00100" )
get.tbl <-
    function( tbl.name, all.tables ) {

        for( i in 1 : length( all.tables ) )

            if( all.tables[[ i ]]$name == tbl.name )

                return( all.tables[[ i ]]$table )

        return( list( NULL ) )
    }


#' read.all.xlsx.tables
#'
#' @param directory the directory containing the xlsx files
#'
#' @return all xlsx tables in the directory
#' @export
#'
#' @examples read.all.xlsx( )
#' tbls
read.all.xlsx.tables <-
    function( directory = "." ) {
        old.wd <- getwd( )
        setwd( directory )
        all.tables <-
            lapply(
                dir( pattern = "*.xlsx" ),
                function( d ) {
                    list(
                        name = substr( d, 8, 8 + 5 ),
                        table = readxl::read_excel( d ) ) } )
        setwd( old.wd )

        all.tables
    }

all.xlsx.tables.names <-
    function( all.tables )
        unlist( sapply( all.tables, function( el ) el[[ 1 ]][ 1 ] ) )

all.xlsx.tables.row.names <-
    function( all.tables )
        sapply( all.tables, function( el ) names( el[[ 2 ]] ) )

