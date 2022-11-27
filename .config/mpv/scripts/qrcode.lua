local qrencode = '/usr/bin/qrencode'
local temp_file = '/tmp/qrencode_mpv'
local utils = require 'mp.utils'

function show_qrcode()
    local table = { args = { 'feh' } }
    local a = table.args
    a[#a + 1] = temp_file
    local result = utils.subprocess(table)
    mp.msg.warn('Qr code shown')
end

function generate_qrcode()
    local table = { args = { qrencode } }
    local a = table.args
    local url = mp.get_property('path')
    local time_pos = mp.get_property('time-pos')
    a[#a + 1] = string.format("%s&t=%d", url, time_pos)
    a[#a + 1] = '-o'
    a[#a + 1] = temp_file
    local result = utils.subprocess(table)
    mp.osd_message('Qr code createdDone')
    show_qrcode()
end

mp.add_key_binding('y', 'generate_qrcode', generate_qrcode)
