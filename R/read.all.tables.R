my <- new.env( )

#' get.tbl
#'
#' @param name name of table eg. D00100
#'
#' @return the requested table
#' @export
#'
#' @examples get.tbl.by.name( "D00100" )
get.table.by.name <-
    function( name ) {

        for( i in 1 : length( my$tables ) )

            if( my$tables[[ i ]]$name == name )

                return( my$tables[[ i ]]$data )

        return( list( NULL ) )
    }

#' get.table.by.index
#'
#' @param index
#'
#' @return the requested table
#' @export
#'
#' @examples get.table.by.index( 2 )
get.table.by.index <-
    function( index  ) {
        return( my$tables[[ index ]]$data )
    }


#' read.all.tables
#'
#' @param directory the directory containing the data files
#'
#' @return none
#' @export
#'
#' @examples read.all.tables( "*.xlsx" )
#' my$name
read.all.tables <-
    function( directory = ".", pattern = "*.xlsx" ) {
        old.wd <- getwd( )
        setwd( directory )
        tmp <-
            lapply(
                dir( pattern = pattern ),
                function( d ) {
                    list(
                        name = substr( d, 8, 8 + 5 ),
                        data = readxl::read_excel( d )
                    )
                }
            )
        my$tables <- tmp
        my$names  <- unlist( sapply( my$tables, function( el ) el[[ 1 ]][ 1 ] ) )
        my$colums <- sapply( my$tables, function( el ) names( el[[ 2 ]] ) )
    }
