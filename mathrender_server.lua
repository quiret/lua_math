-- Created by The_GTA. If you use this please mention my name somewhere.
-- Script to create a render buffer out of a scene.
local string = string;

local function to_color_item(a, r, g, b)
    return string.char(b) .. string.char(g) .. string.char(r) .. string.char(a);
end

local function color_idx(x, y, width)
    return ( ( y * width ) + x ) + 1;
end

-- Creates a backbuffer which is a string of bytes that can have certain dimensions.
-- Can be merged using table.concat to be fed to texture engines.
local function create_backbuffer( width, height, alphaClear, redClear, greenClear, blueClear )
    local bbuf = {};
    local items = {};
    
    local clear_color = to_color_item(alphaClear, redClear, greenClear, blueClear);
    
    for y=0,height-1,1 do
        for x=0,width-1,1 do
            local color_idx = color_idx(x, y, width);
        
            items[color_idx] = clear_color;
        end
    end
    
    bbuf.width = width;
    bbuf.height = height;
    bbuf.items = items;
    
    return bbuf;
end

-- Sets the color of a single pixel on the backbuffer, if possible.
local function set_pixel_on_bbuf(bbuf, xpos, ypos, alpha, red, green, blue)
    local width = bbuf.width;
    local height = bbuf.height;
    
    local ypos_int = math.floor(ypos);
    
    if ( ypos_int < 1 ) or ( ypos_int > height ) then
        return false;
    end
    
    local xpos_int = math.floor(xpos);
    
    if ( xpos_int < 1 ) or ( xpos_int > width ) then
        return false;
    end
    
    local cidx = color_idx(xpos_int, ypos_int, width);
    
    bbuf.items[cidx] = to_color_item(alpha, red, green, blue);
    return true;
end

-- Draws a 2d plane, which resides in screen space, on the backbuffer.
-- The plane can be of any form, like a square or triangle.
local function draw_plane_on_bbuf(bbuf, plane, doDebug)
    -- plane is expected to be in screen coordinates.
    local screenPlane = createPlane2D(0, 0, bbuf.width, 0, 0, bbuf.height);
    
    -- Uncomment this to have triangle planes.
    --plane.addSupremumPolynomeU( 1, -1 );
    
    local u_inter, v_inter = plane.intersectPlane2D(screenPlane, doDebug);
    
    if not ( u_inter ) then
        return false;
    end
    
    -- Go through each valid u coordinate and draw all the associated v coordinates.
    local num_drawn_pixels = 0;
    local num_skipped_pixels = 0;
    
    for m,n in ipairs(u_inter) do
        local min_v = n.interval.min;
        local max_v = n.interval.max;
        local min_u = n.min_poly.minInterval(min_v, max_v);
        local max_u = n.max_poly.maxInterval(min_v, max_v);
        
        local diff_u = ( max_u - min_u );
        local diff_v = ( max_v - min_v );
        
        local diff_v_pixels = math.ceil(diff_v * bbuf.height);
        
        local start_x, start_y = screenPlane.transformPoint( min_u, min_v );
        start_y = math.floor(start_y);
        
        for y=0,diff_v_pixels-1,1 do
            local abs_y = ( start_y + y );
            local abs_v = ( abs_y / bbuf.height );

            -- Since we know the dimensions of the to-be-drawn surface we can process by scan-lines.
            local block_min_u = n.min_poly.evaluate( abs_v );
            local block_max_u = n.max_poly.evaluate( abs_v );
            
            local block_diff_u = ( block_max_u - block_min_u );
        
            local block_diff_u_pixels = ( block_diff_u * bbuf.width );
        
            local block_start_x = math.floor( block_min_u * bbuf.width );
        
            for x=0,block_diff_u_pixels-1,1 do
                local abs_x = ( block_start_x + x );
                
                local abs_u = ( abs_x / bbuf.width );
                
                -- Idea: you could look up inside of a texture for each (abs_u, abs_v) tuple and load the real color.
                -- This way we could draw real textured polygons.

                set_pixel_on_bbuf(bbuf, abs_x, abs_y, 255, 100, 100, 100);
                
                num_drawn_pixels = num_drawn_pixels + 1;
            end
        end
    end
    
    return true, num_drawn_pixels, num_skipped_pixels;
end

-- It is actually undocumented, but plain pixels have two unsigned shorts that define the
-- size of the image at the tail.

local function num_to_ushort_bytes(num)
    local integer = math.floor(num);
    local lower = ( integer % 256 );
    local upper = math.floor( integer / 256 ) % 256;
    
    return string.char(lower, upper);
end

-- Some test planes that reside in screen space.
local test_planes = {
    createPlane2D(10, 10, 300, 10, 0, 50),
    createPlane2D(400, 400, 100, 100, 100, -100),
    createPlane2D(800, 800, -150, -100, 10, 50),
    createPlane2D(0, 300, 150, 0, 0, 100)
};

-- Draws a backbuffer and sends it in "plain" format to all clients.
addCommandHandler("send_bbuf", function()        
        local bbuf = create_backbuffer(640, 480, 255, 255, 0, 50);
        
        local doDebug = false;
        
        local time_start = getTickCount();
        
        for m,n in ipairs(test_planes) do
            local gotToDraw, numDrawn, numSkipped = draw_plane_on_bbuf(bbuf, n, doDebug);
            
            if ( gotToDraw ) and ( doDebug ) then
                outputDebugString( "drawn " .. numDrawn .. " pixels (skipped " .. numSkipped .. ")" );
            end
        end
        
        local time_end = getTickCount();
        local ms_diff = ( time_end - time_start );
        
        outputDebugString( "render time: " .. ms_diff .. "ms" );
        
        local bbuf_width_ushort = num_to_ushort_bytes( bbuf.width );
        local bbuf_height_ushort = num_to_ushort_bytes( bbuf.height );
        
        local pixels_str = table.concat(bbuf.items);
        
        local bbuf_string =
            pixels_str ..
            ( bbuf_width_ushort ..
            bbuf_height_ushort );
            
        local players = getElementsByType("player");
        
        for m,n in ipairs(players) do
            triggerClientEvent(n, "onServerTransmitImage", root, bbuf_string);
        end
        
        outputDebugString("sent backbuffer to clients");
    end
);