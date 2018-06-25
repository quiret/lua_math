local function create_plane2d_example1()
    return createPlane2D(
        3, 4,
        0, 2,
        2, -1
    ), createPlane2D(
        1, 2,
        7, -1,
        1, 6
    );
end

local function create_plane2d_example2()
    return createPlane2D(
        2, 2,
        1, 5,
        2, 0
    ), createPlane2D(
        1, 5,
        0, 2,
        5, -4
    );
end

local function num_equal(a, b)
    return ( math.abs(a-b) < 0.001 );
end

addCommandHandler("plane_test1",
    function()
        local p1, p2 = create_plane2d_example1();
        
        local u_inter, v_inter = p1.intersectPlane2D(p2, true);
        
        -- Checking values from my math paper where I calculated by hand.
        assert( #u_inter == 3 );
        assert( #v_inter == 3 );
        assert( num_equal( u_inter[1].interval.min, ( 22 / 86 ) ) );
        assert( num_equal( u_inter[1].interval.max, ( 32 / 86 ) ) );
        assert( num_equal( u_inter[1].min_poly.getOffset(), ( 6 / 5 ) ) );
        assert( num_equal( u_inter[1].min_poly.getRise(), ( -13 / 5 ) ) );
        assert( num_equal( u_inter[1].max_poly.getOffset(), ( 4 / 7 ) ) );
        assert( num_equal( u_inter[1].max_poly.getRise(), ( -1 / 7 ) ) );
        assert( num_equal( u_inter[2].interval.min, ( 32 / 86 ) ) );
        assert( num_equal( u_inter[2].interval.max, ( 50 / 86 ) ) );
        assert( num_equal( u_inter[2].min_poly.getOffset(), ( 2 / 7 ) ) );
        assert( num_equal( u_inter[2].min_poly.getRise(), ( -1 / 7 ) ) );
        assert( num_equal( u_inter[2].max_poly.getOffset(), ( 4 / 7 ) ) );
        assert( num_equal( u_inter[2].max_poly.getRise(), ( -1 / 7 ) ) );
        assert( num_equal( u_inter[3].interval.min, ( 50 / 86 ) ) );
        assert( num_equal( u_inter[3].interval.max, ( 60 / 86 ) ) );
        assert( num_equal( u_inter[3].min_poly.getOffset(), ( 2 / 7 ) ) );
        assert( num_equal( u_inter[3].min_poly.getRise(), ( -1 / 7 ) ) );
        assert( num_equal( u_inter[3].max_poly.getOffset(), 2 ) );
        assert( num_equal( u_inter[3].max_poly.getRise(), ( -13 / 5 ) ) );
        
        -- TODO: check the intervals and polynomes for v2 aswell.
    end
);

addCommandHandler("plane_test2",
    function()
        -- Note: I calculated it the other way round on the math paper, determining
        -- polynomes in u1/v1, so the planes have been flipped here.
        local p1, p2 = create_plane2d_example2();
    
        local u_inter, v_inter = p1.intersectPlane2D( p2, true );
        
        assert( #u_inter == 3 );
        assert( #v_inter == 1 );
        
        -- Check properties of u2.
        assert( num_equal( u_inter[1].interval.min, ( 8 / 29 ) ) );
        assert( num_equal( u_inter[1].interval.max, ( 10 / 29 ) ) );
        assert( num_equal( u_inter[1].min_poly.getOffset(), 0 ) );
        assert( num_equal( u_inter[1].min_poly.getRise(), 0 ) );
        assert( num_equal( u_inter[1].max_poly.getOffset(), -4 ) );
        assert( num_equal( u_inter[1].max_poly.getRise(), ( 29 / 2 ) ) );
        assert( num_equal( u_inter[2].interval.min, ( 10 / 29 ) ) );
        assert( num_equal( u_inter[2].interval.max, ( 18 / 29 ) ) );
        assert( num_equal( u_inter[2].min_poly.getOffset(), 0 ) );
        assert( num_equal( u_inter[2].min_poly.getRise(), 0 ) );
        assert( num_equal( u_inter[2].max_poly.getOffset(), 1 ) );
        assert( num_equal( u_inter[2].max_poly.getRise(), 0 ) );
        assert( num_equal( u_inter[3].interval.min, ( 18 / 29 ) ) );
        assert( num_equal( u_inter[3].interval.max, ( 20 / 29 ) ) );
        assert( num_equal( u_inter[3].min_poly.getOffset(), -9 ) );
        assert( num_equal( u_inter[3].min_poly.getRise(), ( 29 / 2 ) ) );
        assert( num_equal( u_inter[3].max_poly.getOffset(), 1 ) );
        assert( num_equal( u_inter[3].max_poly.getRise(), 0 ) );
        
        -- Check properties of v2.
        assert( num_equal( v_inter[1].interval.min, 0 ) );
        assert( num_equal( v_inter[1].interval.max, 1 ) );
        assert( num_equal( v_inter[1].min_poly.getOffset(), ( 8 / 29 ) ) );
        assert( num_equal( v_inter[1].min_poly.getRise(), ( 2 / 29 ) ) );
        assert( num_equal( v_inter[1].max_poly.getOffset(), ( 18 / 29 ) ) );
        assert( num_equal( v_inter[1].max_poly.getRise(), ( 2 / 29 ) ) );
    end
);

-- TODO: add example tests for triangle plane, circle plane.