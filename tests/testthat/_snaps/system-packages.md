# async_system_list_packages_dpkg_query

    Code
      ans
    Output
      # A data frame: 3 x 4
        status package      version      provides 
        <chr>  <chr>        <chr>        <list>   
      1 ii     adduser      3.118ubuntu5 <chr [0]>
      2 ii     apt          2.4.9        <chr [1]>
      3 ii     x11proto-dev 2021.5-1     <chr [4]>

# async_system_list_packages_rpm

    Code
      ans
    Output
      # A data frame: 7 x 4
        status package                  version                  provides  
      * <chr>  <chr>                    <chr>                    <list>    
      1 ii     crypto-policies          crypto-policies          <chr [1]> 
      2 ii     gpg-pubkey               gpg-pubkey               <chr [5]> 
      3 ii     libgcc                   libgcc                   <chr [16]>
      4 ii     ncurses-base             ncurses-base             <chr [0]> 
      5 ii     pcre2-syntax             pcre2-syntax             <chr [0]> 
      6 ii     python3-setuptools-wheel python3-setuptools-wheel <chr [7]> 
      7 ii     tzdata                   tzdata                   <chr [0]> 

