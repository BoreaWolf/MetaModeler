# 
# Author: Riccardo Orizio (R)
# Date: Tue 22 Aug 2017 
# 
# Description: Selecting the best modes based on a scoring function
# 
# The code may be used for academic, non-commercial purposes only.
# 

# ******* Libraries *******
source( "~/Projects/R/Useful/UsefulFunctions.R" )

# ******* Functions *******
# TODO: Improve this to read from file or input in order to have whatever modes
# I want
# TODO: Maybe add also a name to the mode
get_modes <- function()
{
	mode1 <- c( 2, 3 )
	mode2 <- c( -5, 0, 4 )
	mode3 <- c( 4, -2 )

	modes <- list()
	modes[[ length( modes ) + 1 ]] <- mode1
	modes[[ length( modes ) + 1 ]] <- mode2
	modes[[ length( modes ) + 1 ]] <- mode3

	return( modes )
}

get_mode <- function( i )
{
	return( get_modes()[[ i ]] )
}

get_modes_length <- function()
{
	return( length( get_modes() ) )
}

# Composing a matrix out of all known modes
get_matrix_modes <- function()
{
	# Finding the maximum length through all modes
	modes <- get_modes()
	lengths <- lapply( modes, length )
	result <- matrix( , nrow = get_modes_length(), ncol = max( unlist( lengths ) ) )
	
	for( i in 1:get_modes_length() )
	{
		# Extending the modes where needed
		if( length( modes[[ i ]] ) < ncol( result ) )
			modes[[ i ]][ ( length( modes[[ i ]] ) + 1 ) : ncol( result ) ] <- 0
		# Saving in the final matrix
		result[ i, ] <- modes[[ i ]]
	}

	return( result )
}

# Array indexes start from 1 => smart choice..
get_mode_function <- function( values )
{
	return( function( x )
			{
				result <- 0.0
				for( i in 1 : length( values ) )
					result = result + values[ i ] * x ^ ( i - 1 )
				return( result )
			} )
}

get_probability <- function( i )
{
	prob <- 0
	switch( i,
			prob <- 0.93,
			prob <- 0.06,
			prob <- 0.01 )

	return( prob )
}

get_cumulative_prob <- function( i )
{
	prob <- 0
	for( i in 1:i )
		prob = prob + get_probability( i )

	return( prob )
}

print_modes <- function( data )
{
	to_print <- data.frame( "Time" = seq( 0, 50, by = 2 ) )
	for( i in seq( 1, get_modes_length() ) )
		to_print[ sprintf( "Mode%d", i ) ] <- lapply( to_print[ "Time" ], get_mode_function( get_mode( i ) ) )
	to_print[ "Simulated" ] <- data

	print( to_print )
	print_graph( to_print, "Time", "Modes" )
}

load_real_data <- function( filename )
{
	# Column names
	data_columns <- c( "Time", "Sensor_1", "Sensor_2", "Sensor_3" )

	# Reading data and model from files
	real_data <- read.csv( filename )
	colnames( real_data ) <- data_columns

	return( real_data )
}

get_nth_combination <- function( n, bits )
{
	# Transforming the number in binary
	return( number2binary( n, bits ) )
}

number2binary <- function( number, bits )
{
	binary_vector <- rev( as.numeric( intToBits( number ) ) )
	if( missing( bits ) ) {
	   return( binary_vector )
	} else {
	   return( binary_vector[ -(1:(length(binary_vector) - bits)) ] )
	}
}

bits_required <- function( number )
{
	cat( number, ceiling( log10( number )/log10(2) ), "\n" )
	return( ceiling( log10( number ) / log10( 2 ) ) )
}

# I receive an array showing which modes to use
# TODO: The combination values can be used to change the weight of each mode
# being part of it
combine_modes <- function( combination )
{
	# Creating the result array containing the coefficients of the combined mode
	return( colSums( get_matrix_modes() * combination ) )
}

# Calculating the score of the current solution
calculate_score <- function( current, real )
{
	return( colMeans( current - real )^2 )
}

# ******* Main *******

# Doing some iterations over the known modes considering their probability of
# happening in order to create the meta-mode
iterations <- 100
result_data <- data.frame( "Time" = seq( 0, 50, by = 2 ) )
modes_counter <- rep( 0, get_modes_length() )

for( i in 1:iterations )
{
	random <- runif( 1, 0, 1 )
	for( j in 1:get_modes_length() )
		if( random <= get_cumulative_prob( j ) )
		{
			modes_counter[ j ] = modes_counter[ j ] + 1
			result_data[ sprintf( "Trial%d", i ) ] <- lapply( result_data[ "Time" ], get_mode_function( get_mode( j ) ) )
			break
		}
}

cat( "Data dimension: [", dim( result_data ), "]\n" )
cat( "Simulation distribution on modes: [", modes_counter, "]\n" )

# Removing the time column for the average calculation
result <- rowMeans( result_data[ , -1 ] )

cat( "Data mean: [", result, "]\n" )
# Showing which modes are currently loaded, also adding the average data as last
# column
print_modes( result )

###############################################################################

cat( "####################################################################\n" )

# Reading data from an external file
input_args <- commandArgs( trailingOnly = TRUE )
real_data <- load_real_data( input_args[ 1 ] )
# Deleting the last column, don't need `system_inflow` data right now
real_data[ , ncol( real_data ) ] <- NULL

# Printing data on both console and graphically on pdf
print( real_data )
print_graph( real_data, "Time", "Input data" )

# The -1 skips the null combination where no modes are chosen in account
modes_alt = 2^get_modes_length() - 1

# Storing all data used
results <- list()

# Finding the best combination of modes for each sensor data
for( i in 1:( ncol( real_data ) - 1 ) )
{
	results[[ i ]] <- list()
	results[[ i ]][[ "RealData" ]] <- data.frame( "Time" = real_data[ "Time" ],
												  "Sensor" = real_data[ i+1 ] )
	results[[ i ]][[ "Combinations" ]] <- data.frame( "Time" = real_data[ "Time" ] )
	results[[ i ]][[ "Scores" ]] <- rep( -1, modes_alt )

	# Finding all combinations
	for( j in 1:modes_alt )
	{
		# Getting the current combination
		current_combination <- combine_modes( get_nth_combination( j, get_modes_length() ) )

		results[[ i ]][[ "Combinations" ]][ toString( current_combination ) ] <- 
			lapply( results[[ i ]][[ "Combinations" ]][ "Time" ], get_mode_function( current_combination ) )

		results[[ i ]][[ "Scores" ]][ j ] <- 
			calculate_score( results[[ i ]][[ "Combinations" ]][ toString( current_combination ) ],
							 results[[ i ]][[ "RealData" ]][ sprintf( "Sensor_%d", i ) ] )
	}

	#	print( results )
	print_graph( data.frame( results[[ i ]][[ "RealData" ]],
							 results[[ i ]][[ "Combinations" ]][ , -1 ] ),
							 "Time", sprintf( "Sensor_%d", i ) )

	best_combination = which.min( results[[ i ]][[ "Scores" ]] )

	cat( "Best combination: ", 
		 best_combination,
		 " (",
		 get_nth_combination( best_combination, get_modes_length() ),
		 ") => [",
		 combine_modes( get_nth_combination( best_combination, get_modes_length() ) ),
		 "] with score ",
		 results[[ i ]][[ "Scores" ]][ best_combination ],
		 "\n" )
	print( results[[ i ]][[ "Scores" ]] )
}

#	print( results )



