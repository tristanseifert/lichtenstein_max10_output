# MAX10 FPGA Output Driver
An FPGA-based output driver for the Lichtenstein hardware. This implements a simple "framebuffer" which accepts RGB(W) data over SPI, then outputs it to individual channels.

## SPI Protocol
The first byte of an SPI transaction is the command byte: only the two least significant bits are considered.

### Status Request (0x00)
Upon receipt of this command, two bytes are shifted out from the device, indicating the output status of all 16 channels. The first byte contains the status for channels 16 through 9, whereas the second byte contains channels 8 through 1.

A bit is set if the channel is outputting data, clear otherwise.

### Framebuffer Write (0x01)
Writes one or more bytes into the framebuffer memory. After the command, the device expects a 24-bit address, in big endian byte order, followed by one or more bytes of data to write to the memory.

The address is automatically incremented, and writes continue one byte at a time until the chip select line is de-asserted to indicate completion of the command.

<table style="text-align: center;">
	<tr>
		<th>0</th>
		<th>1</th>
		<th>2</th>
		<th>3</th>
		<th>3+N</th>
	</tr>
	<tr>
		<td colspan="3">Address</td>
		<td colspan="2">Data</td>
	</tr>
</table>

### Register Write (0x02)
Latches the given address and data length into the given channel's output registers. The device expects the byte channel number, a 24-bit address, and a 16-bit length, all in big endian:

<table style="text-align: center;">
	<tr>
		<th>0</th>
		<th>1</th>
		<th>2</th>
		<th>3</th>
		<th>4</th>
		<th>5</th>
	</tr>
	<tr>
		<td>Channel</td>
		<td colspan="3">Address</td>
		<td colspan="2">Length</td>
	</tr>
</table>
