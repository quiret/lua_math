-- Created by The_GTA for research purposes. Use it for free but mention me somewhere pls.
--[[
    A 2D plane is a pair of vectors that span an encapsulated area.
    Points are inside the plane if they are between the upper and lower polynomes of the U and V dimensions.
    Basically if you have a regular plane of u,v in [0,1] and draw the four vectors of it, all points inside the closed
    form meet the criterium.
    
    We are using basic linear math to solve this. A more optimized form is found inside any GPU.
    
    Note that a plane is invalid in our case if it is unbounded in any direction.
]]
function createPlane2D( p1x, p1y, u1x, u1y, v1x, v1y )
    local plane = {};
    
    -- Obtaining all plane vectors.
    function plane.getX()
        return p1x;
    end
    
    function plane.getY()
        return p1y;
    end
    
    function plane.getUX()
        return u1x;
    end
    
    function plane.getUY()
        return u1y;
    end
    
    function plane.getVX()
        return v1x;
    end
    
    function plane.getVY()
        return v1y;
    end
    
    -- Since each plane is basically a 2D matrix, we can transform points inside of it.
    function plane.transformPoint(u, v)
        local new_x = ( u1x * u + v1x * v + p1x );
        local new_y = ( u1y * u + v1y * v + p1y );
        
        return new_x, new_y;
    end
    
    -- Calculates the determinant of 2D matrix.
    local function calcdet2( ux, uy, vx, vy )
        return ( ux * vy - uy * vx );
    end
    
    -- Create a simple polynomes of the form: offset + coefficient * x
    local function create_polynome(off, rise)
        local poly = {};
        
        function poly.getOffset()
            return off;
        end
        
        function poly.getRise()
            return rise;
        end
        
        -- Compares two polynomes less-equal with each other and returns the resulting condition.
        local function minFunc(otherOff, otherRise)
            local risediff = ( rise - otherRise );
        
            if ( risediff == 0 ) then
                -- Condition for when this polynome is always smaller than otherPoly.
                return "all", ( off <= otherOff );
            end
            
            local bound = ( otherOff - off ) / risediff;
            
            if ( risediff > 0 ) then
                -- This polynome is smaller up to the following bound.
                -- Or the second polynome is greater starting from the bound.
                return "upperbound", bound;
            else
                -- This polynome is smaller starting from the following bound.
                -- Or the second polynome is greater up the the bound.
                return "lowerbound", bound;
            end
        end
        
        -- Compares two polynomes greater-equal.
        local function maxFunc(otherOff, otherRise)
            local risediff = ( rise - otherRise );
        
            if ( risediff == 0 ) then
                -- Condition for when this polynome is always smaller than otherPoly.
                return "all", ( off >= otherOff );
            end
            
            local bound = ( otherOff - off ) / risediff;
            
            if ( risediff > 0 ) then
                -- This polynome is bigger starting from the following bound.
                -- Or the second polynome is smaller up to the bound.
                return "lowerbound", bound;
            else
                -- This polynome is bigger up to the following bound.
                -- Or the second polynome is greater starting from the bound.
                return "upperbound", bound;
            end
        end
        
        function poly.minPoly(otherPoly)
            local otherOff = otherPoly.getOffset();
            local otherRise = otherPoly.getRise();
            
            return minFunc( otherOff, otherRise );
        end
        
        function poly.min(otherOff, otherRise)
            return minFunc( otherOff, otherRise );
        end
        
        function poly.maxPoly(otherPoly)
            local otherOff = otherPoly.getOffset();
            local otherRise = otherPoly.getRise();
            
            return maxFunc( otherOff, otherRise );
        end
        
        function poly.max(otherOff, otherRise)
            return maxFunc( otherOff, otherRise );
        end
        
        -- Calculates the minimum value of a domain-bounded polynome.
        function poly.minInterval(domainLow, domainHigh)
            if ( rise < 0 ) then
                return ( off + domainHigh * rise );
            end
            
            return ( off + domainLow * rise );
        end
        
        -- Calculates the maximum value of a domain-bounded polynome.
        function poly.maxInterval(domainLow, domainHigh)
            if ( rise < 0 ) then
                return ( off + domainLow * rise );
            end
            
            return ( off + domainHigh * rise );
        end
        
        -- Calculates the value of the polynome in point.
        function poly.evaluate(point)
            return off + point * rise;
        end
        
        -- Returns the string representation of this polynome.
        function poly.toString()
            return ( "off=" .. off .. ", asc=" .. rise );
        end
        
        return poly;
    end
    
    -- Creates an interval that displays the domain-range of a polynome.
    local function create_interval_link( poly, min, max )
        local link = {};
        
        function link.updateMin(newMin)
            if ( min < newMin ) then
                min = newMin;
            end
        end
        
        function link.updateMax(newMax)
            if ( max > newMax ) then
                max = newMax;
            end
        end
        
        function link.invalidate()
            min = 1;
            max = 0;
        end
    
        function link.isEmpty()
            return ( min > max );
        end
        
        function link.getMin()
            return min;
        end
        
        function link.getMax()
            return max;
        end
        
        function link.getPolynome()
            return poly;
        end
        
        return link;
    end
    
    -- For a given set of polynomes, this function calculates the domains of each one being valid if
    -- being either the infimum (minTrueMaxFalse == false) or the supremum (minTrueMaxFalse == true).
    -- Returns the list of intervals with their polynomes sorted by starting offsets.
    local function calculate_minmax_intervals( minStart, maxStart, minTrueMaxFalse, poly_list )
        local interval_set = {};
        
        -- Initialize all intervals for polynomes.
        do
            local n = 1;
            local num_poly = #poly_list;
            local inter_idx = 1;
            
            while ( n <= num_poly ) do
                local poly = poly_list[ n ];
            
                if ( poly ) then
                    local interval_link = create_interval_link( poly, minStart, maxStart );
                    
                    interval_set[ inter_idx ] = interval_link;
                    
                    inter_idx = inter_idx + 1;
                end
                
                n = n + 1;
            end
        end
        
        local num_intervals = #interval_set;
        
        do
            local n = 1;
            
            while ( n <= num_intervals ) do
                local prim_poly_link = interval_set[ n ];
                local prim_poly = prim_poly_link.getPolynome();
                
                local k = ( n + 1 );
                
                while ( k <= num_intervals ) do
                    local sec_poly_link = interval_set[ k ];
                    local sec_poly = sec_poly_link.getPolynome();
                    
                    if not ( sec_poly_link.isEmpty() ) then
                        -- Perform the function.
                        local boundType, dynVal;
                        
                        if ( minTrueMaxFalse ) then
                            boundType, dynVal = prim_poly.minPoly( sec_poly );
                        else
                            boundType, dynVal = prim_poly.maxPoly( sec_poly );
                        end
                        
                        if ( boundType == "all" ) then
                            if ( dynVal ) then
                                sec_poly_link.invalidate();
                            else
                                prim_poly_link.invalidate();
                            end
                        else
                            if ( boundType == "lowerbound" ) then
                                prim_poly_link.updateMin( dynVal );
                                sec_poly_link.updateMax( dynVal );
                            elseif ( boundType == "upperbound" ) then
                                prim_poly_link.updateMax( dynVal );
                                sec_poly_link.updateMin( dynVal );
                            end
                        end
                        
                        -- Check if the prim poly has turned irrelevant.
                        -- If yes we can stop here.
                        if ( prim_poly_link.isEmpty() ) then
                            break;
                        end
                    end
                
                    k = k + 1;
                end
            
                n = n + 1;
            end
        end
        
        -- Now we have a list of all intervals for all polynomes.
        -- We must exclude the intervals that do not matter tho.
        local relevant_interval_set = {};
        
        do
            local n = 1;
            local newidx = 1;
            
            while ( n <= num_intervals ) do
                local add_poly_link = interval_set[ n ];
                
                if not ( add_poly_link.isEmpty() ) then
                    relevant_interval_set[ newidx ] = add_poly_link;
                    
                    newidx = newidx + 1;
                end
                
                n = n + 1;
            end
        end
        
        -- Sort the intervals, too!
        table.sort( relevant_interval_set,
            function( left_inter, right_inter )
                -- We check maximum too, so that we sort the items in each other in case they are one-off items.
                return ( left_inter.getMin() <= right_inter.getMin() and left_inter.getMax() < right_inter.getMax() );
            end
        );
        
        return relevant_interval_set;
    end
    
    -- Nicely prints the list of intervals and their polynomes.
    local function print_interval_set( intervals )
        for m,n in ipairs(intervals) do
            local poly = n.getPolynome();
            
            local min = n.getMin();
            local max = n.getMax();
            
            outputDebugString( "[" .. min .. "," .. max .. "]: " .. poly.toString() );
        end
    end
    
    -- For a given set of minimum and maximum polynomes, calculates the area at which
    -- each infimum polynome is smaller-equal than the associated supremum polynome.
    -- Returns the shared intervals where infimum and supremum polynomes count,
    -- again sorted by the interval starting offsets.
    local function cut_minmax_intervals(min_intervals, max_intervals)
        -- Note that the intervals have to be sorted (by minimum and maximum).
        local i = 1;
        local j = 1;
        
        local num_min = #min_intervals;
        local num_max = #max_intervals;
        
        local sorted_intrusion_list = {};
        local sorted_intrusion_idx = 1;
        
        while ( i <= num_min ) and ( j <= num_max ) do
            local min_interval = min_intervals[ i ];
            local max_interval = max_intervals[ j ];
        
            -- Find all sections that both polynomes share.
            local sect_start = math.max( min_interval.getMin(), max_interval.getMin() );
            local sect_end = math.min( min_interval.getMax(), max_interval.getMax() );
            
            if ( sect_start <= sect_end ) then
                -- Intersect upper with lower border.
                local min_poly = min_interval.getPolynome();
                local max_poly = max_interval.getPolynome();
                
                local interType, depVal = min_poly.minPoly( max_poly );
                
                local intrude_item = false;
                
                if ( interType == "all" ) then
                    if ( depVal ) then
                        intrude_item = {};
                        intrude_item.min_poly = min_poly;
                        intrude_item.max_poly = max_poly;
                        intrude_item.interval = { min = sect_start, max = sect_end };
                    end
                elseif ( interType == "lowerbound" ) then
                    local new_min = math.max( sect_start, depVal );
                    
                    if ( new_min <= sect_end ) then
                        intrude_item = {};
                        intrude_item.min_poly = min_poly;
                        intrude_item.max_poly = max_poly;
                        intrude_item.interval = { min = new_min, max = sect_end };
                    end
                elseif ( interType == "upperbound" ) then
                    local new_max = math.min( sect_end, depVal );
                    
                    if ( sect_start <= new_max ) then
                        intrude_item = {};
                        intrude_item.min_poly = min_poly;
                        intrude_item.max_poly = max_poly;
                        intrude_item.interval = { min = sect_start, max = new_max };
                    end
                end
                
                if ( intrude_item ) then
                    sorted_intrusion_list[ sorted_intrusion_idx ] = intrude_item;
                    sorted_intrusion_idx = sorted_intrusion_idx + 1;
                end
            end
            
            -- Advance the polynomes that we just passed.
            local didAdvanceMinInterval = false;
            
            if ( min_interval.getMax() <= sect_end ) then
                i = i + 1;
                didAdvanceMinInterval = true;
            end
            
            if ( max_interval.getMax() <= sect_end ) then
                -- We must check that we have no intersection to the current min_interval if
                -- we did advance it.
                local cur_min_interval = min_intervals[ i ];
                
                if not ( didAdvanceMinInterval ) or ( cur_min_interval == nil ) or ( max_interval.getMax() < cur_min_interval.getMin() ) then
                    j = j + 1;
                end
            end
        end
        
        assert( i > num_min );
        assert( j > num_max );
        
        -- Return a sorted list of intervals where the intersection produced actual regions.
        return sorted_intrusion_list;
    end
    
    -- Returns the maximum spanning of the interval set if we just looked
    -- at the start and end intervals.
    local function get_min_max( interval_list )
        local num_items = #interval_list;
    
        if ( num_items == 0 ) then
            return 1, 0;
        end
        
        local first_inter = interval_list[ 1 ];
        local last_inter = interval_list[ num_items ];
        
        return first_inter.interval.min, last_inter.interval.max;
    end
    
    -- Returns the minimum value of the given constant polynomes.
    local function const_min_poly(infipoly_list)
        local min = false;
    
        for m,n in ipairs(infipoly_list) do
            if (n.getRise() == 0) then
                local newMin = n.getOffset();
                
                if (min == false) or (newMin > min) then
                    min = newMin;
                end
            end
        end
        
        return min;
    end
    
    -- Returns the maximum value of the given constant polynomes.
    local function const_max_poly(suppoly_list)
        local max = false;
    
        for m,n in ipairs(suppoly_list) do
            if (n.getRise() == 0) then
                local newMax = n.getOffset();
                
                if (max == false) or (newMax < max) then
                    max = newMax;
                end
            end
        end
        
        return max;
    end
    
    -- Given a set of infimum polynomes and supremum polynomes that enclose an area, this function
    -- calculates the intervals where the polynomes count.
    local function calculate_tight_uv_boundaries(u_infipoly_list, u_suppoly_list, v_infipoly_list, v_suppoly_list)
        local u_validity_min = const_min_poly( u_infipoly_list );
        local u_validity_max = const_max_poly( u_suppoly_list );
        
        local v_validity_min = const_min_poly( v_infipoly_list );
        local v_validity_max = const_max_poly( v_suppoly_list );

        assert(not (u_validity_min == false));
        assert(not (u_validity_max == false));
        
        assert(not (v_validity_min == false));
        assert(not (v_validity_max == false));
        
        local u_min_interval_set = calculate_minmax_intervals( v_validity_min, v_validity_max, false, u_infipoly_list );
        local u_max_interval_set = calculate_minmax_intervals( v_validity_min, v_validity_max, true, u_suppoly_list );
        local v_min_interval_set = calculate_minmax_intervals( u_validity_min, u_validity_max, false, v_infipoly_list );
        local v_max_interval_set = calculate_minmax_intervals( u_validity_min, u_validity_max, true, v_suppoly_list );
        
        return u_min_interval_set, u_max_interval_set, v_min_interval_set, v_max_interval_set;
    end
    
    -- Calculates the equations to display coordinates in the plane 1 as coordinates
    -- in the plane 2.
    local function calculateCoordinateTransform(
        plane1det,
        p1x, p1y, u1x, u1y, v1x, v1y,
        p2x, p2y, u2x, u2y, v2x, v2y
    )
        -- The idea is that we calculate definite equations for both u1 and v1 so that we can insert
        -- them into all resulting inequalities based on u1/v1 coordinates. By doing so we obtain
        -- inequalities in u2/v2 so that we can compare against the inequalities of the second plane.
        -- This way we obtain boundary intervals for each u2/v2 tuple.
    
        -- We assume that both determinants are not zero.
        local pdiff_x = ( p2x - p1x );
        local pdiff_y = ( p2y - p1y );
        
        -- For u2.
        local pdiff_v1_det = calcdet2( pdiff_x, pdiff_y, v1x, v1y );
        local u2_v1_det = calcdet2( u2x, u2y, v1x, v1y );
        local v2_v1_det = calcdet2( v2x, v2y, v1x, v1y );
        
        -- For v2.
        local u1_pdiff_det = calcdet2( u1x, u1y, pdiff_x, pdiff_y );
        local u1_u2_det = calcdet2( u1x, u1y, u2x, u2y );
        local u1_v2_det = calcdet2( u1x, u1y, v2x, v2y );
        
        -- Equations.
        local u1_off = pdiff_v1_det / plane1det;
        local u1_u2mod = u2_v1_det / plane1det;
        local u1_v2mod = v2_v1_det / plane1det;
        
        local v1_off = u1_pdiff_det / plane1det;
        local v1_u2mod = u1_u2_det / plane1det;
        local v1_v2mod = u1_v2_det / plane1det;
        
        return u1_off, u1_u2mod, u1_v2mod, v1_off, v1_u2mod, v1_v2mod;
    end
    
    -- Given a boundary polynome in plane 1, calculates the transformed boundary polynome
    -- in plane 2 coordinates.
    local function transformBoundary_u2(
        off, rise,
        u1_off, u1_u2mod, u1_v2mod,
        v1_off, v1_u2mod, v1_v2mod,
        u2_infipoly_list, u2_suppoly_list,
        v2_infipoly_list, v2_suppoly_list,
        doUpperOrLower
    )
        local addedOneBound = false;
        
        local bound_off_mod = ( off + rise * v1_off - u1_off );
        
        -- Calculate boundary in u2.
        do
            local mod = ( u1_u2mod - rise * v1_u2mod );
            
            if not ( mod == 0 ) then
                local bound_off = bound_off_mod / mod;
                local bound_v2mod = -( u1_v2mod - rise * v1_v2mod ) / mod;
                
                local trueUpperFalseLower = ( mod > 0 );
                
                if not ( doUpperOrLower ) then
                    trueUpperFalseLower = not trueUpperFalseLower;
                end
                
                local poly = create_polynome( bound_off, bound_v2mod );
                
                if ( trueUpperFalseLower ) then
                    table.insert(u2_suppoly_list, poly);
                else
                    table.insert(u2_infipoly_list, poly);
                end
                
                addedOneBound = true;
            end
        end
        
        -- Calculate boundary in v2.
        do
            local mod = ( u1_v2mod - rise * v1_v2mod );
            
            if not ( mod == 0 ) then
                local bound_off = bound_off_mod / mod;
                local bound_u2mod = -( u1_u2mod - rise * v1_u2mod ) / mod;
                
                local trueUpperFalseLower = ( mod > 0 );
                
                if not ( doUpperOrLower ) then
                    trueUpperFalseLower = not trueUpperFalseLower;
                end
                
                local poly = create_polynome( bound_off, bound_u2mod );
                
                if ( trueUpperFalseLower ) then
                    table.insert(v2_suppoly_list, poly);
                else
                    table.insert(v2_infipoly_list, poly);
                end
                
                addedOneBound = true;
            end
        end
        
        assert( addedOneBound == true );
    end

    -- Same as above but for the V coordinate of the second plane.
    local function transformBoundary_v2(
        off, rise,
        u1_off, u1_u2mod, u1_v2mod,
        v1_off, v1_u2mod, v1_v2mod,
        u2_infipoly_list, u2_suppoly_list,
        v2_infipoly_list, v2_suppoly_list,
        doUpperOrLower
    )
        local addedOneBound = false;
        
        local bound_off_mod = ( off + rise * u1_off - v1_off );
        
        -- Boundaries based on u2.
        do
            local mod = ( v1_u2mod - rise * u1_u2mod );
            
            if not ( mod == 0 ) then
                local bound_off = bound_off_mod / mod;
                local bound_v2mod = -( v1_v2mod - rise * u1_v2mod ) / mod;
                
                local poly = create_polynome( bound_off, bound_v2mod );
                
                local trueUpperFalseLower = ( mod > 0 );
                
                if not ( doUpperOrLower ) then
                    trueUpperFalseLower = not trueUpperFalseLower;
                end
                
                if ( trueUpperFalseLower ) then
                    table.insert( u2_suppoly_list, poly );
                else
                    table.insert( u2_infipoly_list, poly );
                end
                
                addedOneBound = true;
            end
        end
        
        -- Boundaries based on v2.
        do
            local mod = ( v1_v2mod - rise * u1_v2mod );
            
            if not ( mod == 0 ) then
                local bound_off = bound_off_mod / mod;
                local bound_u2mod = -( v1_u2mod - rise * u1_u2mod ) / mod;
                
                local poly = create_polynome( bound_off, bound_u2mod );
                
                local trueUpperFalseLower = ( mod > 0 );
                
                if not ( doUpperOrLower ) then
                    trueUpperFalseLower = not trueUpperFalseLower;
                end
                
                if ( trueUpperFalseLower ) then
                    table.insert( v2_suppoly_list, poly );
                else
                    table.insert( v2_infipoly_list, poly );
                end
                
                addedOneBound = true;
            end
        end
        
        assert( addedOneBound == true );
    end
    
    -- Cached immutable polynomes.
    -- Good because these objects are allocated memory (probably bad in C++ case, not required).
    local const_one_poly = create_polynome( 1, 0 );
    local const_zero_poly = create_polynome( 0, 0 );
    
    -- Polynomes that define the plane area.
    -- Can be dynamically added and will result in different areas.
    local u2_plane_suppolys = { const_one_poly };
    local v2_plane_suppolys = { const_one_poly };
    
    local u2_plane_infipolys = { const_zero_poly };
    local v2_plane_infipolys = { const_zero_poly };
    
    -- Adds a minimum border for the U coordinate of points inside of this plane.
    function plane.addInfimumPolynomeU( off, rise )
        table.insert( u2_plane_infipolys, create_polynome( off, rise ) );
    end
    
    -- Adds a minimum border of the the V coordinate of points inside of this plane.
    function plane.addInfimumPolynomeV( off, rise )
        table.insert( v2_plane_infipolys, create_polynome( off, rise ) );
    end

    -- Adds a maximum border of the U coordinate of points inside of this plane.
    function plane.addSupremumPolynomeU( off, rise )
        table.insert( u2_plane_suppolys, create_polynome( off, rise ) );
    end
    
    -- Adds a maximum border of the V coordinate of points inside of this plane.
    function plane.addSupremumPolynomeV( off, rise )
        table.insert( v2_plane_suppolys, create_polynome( off, rise ) );
    end
    
    function plane.getInfimumPolynomesU()
        return u2_plane_infipolys;
    end
    
    function plane.getInfimumPolynomesV()
        return v2_plane_infipolys;
    end
    
    function plane.getSupremumPolynomesU()
        return u2_plane_suppolys;
    end
    
    function plane.getSupremumPolynomesV()
        return v2_plane_suppolys;
    end
    
    -- Calculates the shared region of two planes and returns the polynomes that encapsulate the shared
    -- region in coordinate of the second plane. Returns false if otherwise.
    function plane.intersectPlane2D(otherPlane, doDebug)
        local p2x = otherPlane.getX();
        local p2y = otherPlane.getY();
        local u2x = otherPlane.getUX();
        local u2y = otherPlane.getUY();
        local v2x = otherPlane.getVX();
        local v2y = otherPlane.getVY();
        
        -- I had a crazy idea about matrix multiplication and transformation into quaders and shit to simply much of this math.
        -- It is not rather a simplification of this but... make it more awesome!
        
        local plane1det = calcdet2( u1x, u1y, v1x, v1y );
        local plane2det = calcdet2( u2x, u2y, v2x, v2y );
        
        if ( plane1det == 0 ) or ( plane2det == 0 ) then
            return false;
        end
        
        -- Calculate the transformation matrix.
        local u1_off, u1_u2mod, u1_v2mod, v1_off, v1_u2mod, v1_v2mod
            =
            calculateCoordinateTransform(
                plane1det,
                p1x, p1y, u1x, u1y, v1x, v1y,
                p2x, p2y, u2x, u2y, v2x, v2y
            );
        
        -- Now get the transformed polynomes.
        local function clone_table(src)
            local tab = {};
            
            for m,n in ipairs(src) do
                table.insert(tab, n);
            end
            
            return tab;
        end
        
        -- We store the polynomes that have been transformed from our plane here.
        local u2_trans_infipoly_list = clone_table(otherPlane.getInfimumPolynomesU());
        local u2_trans_suppoly_list = clone_table(otherPlane.getSupremumPolynomesU());
        local v2_trans_infipoly_list = clone_table(otherPlane.getInfimumPolynomesV());
        local v2_trans_suppoly_list = clone_table(otherPlane.getSupremumPolynomesV());
        
        for m,n in ipairs(u2_plane_suppolys) do
            transformBoundary_u2(
                n.getOffset(), n.getRise(), 
                u1_off, u1_u2mod, u1_v2mod,
                v1_off, v1_u2mod, v1_v2mod,
                u2_trans_infipoly_list, u2_trans_suppoly_list,
                v2_trans_infipoly_list, v2_trans_suppoly_list,
                true
            );
        end
        
        for m,n in ipairs(v2_plane_suppolys) do
            transformBoundary_v2(
                n.getOffset(), n.getRise(), 
                u1_off, u1_u2mod, u1_v2mod,
                v1_off, v1_u2mod, v1_v2mod,
                u2_trans_infipoly_list, u2_trans_suppoly_list,
                v2_trans_infipoly_list, v2_trans_suppoly_list,
                true
            );
        end
        
        for m,n in ipairs(u2_plane_infipolys) do
            transformBoundary_u2(
                n.getOffset(), n.getRise(), 
                u1_off, u1_u2mod, u1_v2mod,
                v1_off, v1_u2mod, v1_v2mod,
                u2_trans_infipoly_list, u2_trans_suppoly_list,
                v2_trans_infipoly_list, v2_trans_suppoly_list,
                false
            );
        end
        
        for m,n in ipairs(v2_plane_infipolys) do
            transformBoundary_v2(
                n.getOffset(), n.getRise(), 
                u1_off, u1_u2mod, u1_v2mod,
                v1_off, v1_u2mod, v1_v2mod,
                u2_trans_infipoly_list, u2_trans_suppoly_list,
                v2_trans_infipoly_list, v2_trans_suppoly_list,
                false
            );
        end
        
        -- For all polynomes with constants, we know that they directly translate into conditions to u2.
        -- So check them.
        
        local plane2ueq_min_interval_set, plane2ueq_max_interval_set,
            plane2veq_min_interval_set, plane2veq_max_interval_set
            =
            calculate_tight_uv_boundaries(
                u2_trans_infipoly_list, u2_trans_suppoly_list,
                v2_trans_infipoly_list, v2_trans_suppoly_list
            );
        
        if (doDebug) then
            outputDebugString( "num_plane2ueq_min: " .. #plane2ueq_min_interval_set );
            print_interval_set(plane2ueq_min_interval_set);
            outputDebugString( "num_plane2ueq_max: " .. #plane2ueq_max_interval_set );
            print_interval_set(plane2ueq_max_interval_set);
            outputDebugString( "num_plane2veq_min: " .. #plane2veq_min_interval_set );
            print_interval_set(plane2veq_min_interval_set);
            outputDebugString( "num_plane2veq_max: " .. #plane2veq_max_interval_set );
            print_interval_set(plane2veq_max_interval_set);
        end
        
        -- Cut the min with the max intervals.
        local u_inter_list = cut_minmax_intervals( plane2ueq_min_interval_set, plane2ueq_max_interval_set );
        local v_inter_list = cut_minmax_intervals( plane2veq_min_interval_set, plane2veq_max_interval_set );
        
        if (doDebug) then
            outputDebugString("num_u_inter: " .. #u_inter_list);
            print_interval_cuts(u_inter_list);
            outputDebugString("num_v_inter: " .. #v_inter_list);
            print_interval_cuts(v_inter_list);
        end
        
        local abs_u_min, abs_u_max = get_min_max( u_inter_list );
        local abs_v_min, abs_v_max = get_min_max( v_inter_list );
        
        if ( abs_u_min > abs_u_max ) or ( abs_v_min > abs_v_max ) then
            -- There is no actual intersection.
            return false;
        end
        
        return u_inter_list, v_inter_list;
    end
    
    return plane;
end

function print_interval_cuts(cutlers_list)
    -- I hereby name the list of interval cuts after Dave N Cutler!
    for m,n in ipairs(cutlers_list) do
        local interval = n.interval;
        local min = interval.min;
        local max = interval.max;
        
        local min_poly = n.min_poly;
        local max_poly = n.max_poly;
        
        outputDebugString("[" .. min .. "," .. max .. "]: min_poly=(" .. min_poly.toString() .. "), max_poly=(" .. max_poly.toString() .. ")");
    end
end

-- TODO: add commands for debugging.