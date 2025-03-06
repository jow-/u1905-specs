#!/usr/bin/env ucode

'use strict';

import { open, writefile } from 'fs';

let output = '';
let prev_pad = '';
let prev_code = '';

function println(indent, fmt, ...vals) {
	let nl = (substr(fmt, -1) === '\r') ? "" : "\n";
	fmt = replace(fmt, /\r$/, '');

	let line = '';
	for (let i = 0; i < indent; i++) line += '\t';
	line += sprintf(fmt, ...vals) + nl;

	let m = match(line, /^(\t*)(.*)$/);
	let pad = m?.[1], code = m?.[2];

	let add_blank_line = (
		(nl !== '' && fmt === '') ||
		(match(code, /^(for|if|while) /) && !match(output, /\{\n$/)) ||
		(match(code, /^(let|const) /) && !match(prev_code, /^(let|const) /) && !match(output, /\{\n$/)) ||
		(pad && !match(code, /^[]}]/) && length(pad) < length(prev_pad) && !match(output, /\{\n$/) && !match(code, /^\);/)) ||
		(match(output, /\}\n$/) && !match(code, /^\}/)) ||
		(match(prev_code, /^(\);|let |const )/) && !match(code, /^(\}|let |const )/))
	);

	if (add_blank_line && !match(output, /\n\n$/)) {
		output += "\n";
	}

	if (match(fmt, /}$/)) {
		output = replace(output, /\}\n[ \t]*\n$/, "}\n");
	}

	if (fmt !== '') {
		output += line;
	}

	prev_pad = pad;
	prev_code = code;
}

function bitstr(num)
{
	let res = '';

	for (; num > 0; num /= 2)
		res = (num % 2) + res;

	return substr('00000000' + res, -8);
}

function gen_prop_assign(indent, fields) {
	let bitfield_counter = '';
	let fields_filtered = filter(fields, f => f.type !== 'void' && !('ref' in f));

	if (length(fields_filtered) === 1 && !('enum' in fields_filtered[0])) {
		println(0, fields_filtered[0].name + "\r");
		return;
	}

	println(0, "{");

	for (let field in fields_filtered) {
		println(indent, "	%s,", field.name);

		if ('enum' in field) {
			println(indent, "	%s_name: defs.%s[%s],", field.name, uc(field.enum_name ?? field.name), field.name);
		}
	}

	println(indent, "}\r");
}

function gen_require_condition(requires) {
	let and_cond = [];

	if (type(requires) === "object") {
		for (let ref_id in sort(keys(requires))) {
			let ref_field = requires[ref_id].field;
			let values = requires[ref_id].values;
			if (length(values) === 0) continue;

			if (length(values) === 1) {
				push(and_cond, sprintf('%s == %d', ref_field.name, values[0]));
			}
			else {
				push(and_cond, sprintf('%s in [ %s ]', ref_field.name, join(', ', values)));
			}
		}
	}

	return length(and_cond) ? join(' && ', and_cond) : '';
}

function gen_decode_bitfield(indent, counter, members) {
	let offset = 0;

	println(0, '');
	println(indent, "const bitfield%s = buf.get('B');", counter);

	for (let member in members) {
		let bit_num = member.size / 0.125;
		let bit_off = offset / 0.125;

		offset += member.size;

		if (member.type === 'void') continue;

		if (bit_num === 1.0) {
			println(indent, "const %s = ((bitfield%s & 0b%s) == 0b%s);",
				member.name, counter,
				bitstr((1 << (8 - bit_off - 1))), bitstr((1 << (8 - bit_off - 1))));
		}
		else if (bit_off + bit_num < 8.0) {
			println(indent, "const %s = (bitfield%s >> %d) & 0b%s;",
				member.name, counter,
				(8 - bit_off - bit_num),
				bitstr((1 << bit_num) - 1));
		}
		else {
			println(indent, "const %s = bitfield%s & 0b%s;",
				member.name, counter,
				bitstr((1 << bit_num) - 1));
		}
	}

	println(0, '');
}

function gen_decode_prop_lowlevel(indent, field, optional) {
	let decl = optional ? '' : 'const ';
	const int_fmts = [null, 'B', '!H', null, '!L', null, null, null, '!Q'];
	let m;

	if (field.type === 'array') {
		if ((m = match(field.subtype, /^uint(8|16|32|64)_t$/)) == null)
			die(`Unexpected array subtype for field ${field}`);

		assert(field.size_field != null, `No related array length field for field ${field}`);

		let itervar = 'f';
		for (let i = 0; i <= indent; i++) itervar = chr(ord(itervar) + 1);

		println(indent, "%s%s = [];", decl, field.name);
		println(indent, "for (let %s = 0; %s < %s; %s++) {", itervar, itervar, field.size_field.name, itervar);
		println(indent, "	if (buf.pos() + %d > end)", m[1] / 8);
		println(indent, "		return null;");
		println(indent, "	push(%s, buf.get('%s'));", field.name, int_fmts[m[1] / 8]);
		println(indent, "}");

		//println(indent, "if (buf.pos() + %s > end)", field.size_field.name);
		//println(indent, "	return null;");
		//println(indent, "%s%s = buf.read(`${%s}B`);", decl, field.name, field.size_field.name);
	}
	else if (field.type in ['number', 'boolean'] && (m = match(field.subtype, /^(uint(8|16|32|64)_t|boolean)$/)) != null) {
		const fmt = (m[1] == 'boolean') ? '?' : (m[2] ? int_fmts[m[2] / 8] : 'B');
		println(indent, "%s%s = buf.get('%s');", decl, field.name, fmt);
	}
	else if (field.type === 'string') {
		if (field.subtype === 'mac') {
			println(indent, "%s%s = sprintf('%%02x:%%02x:%%02x:%%02x:%%02x:%%02x', ...buf.read('6B'));", decl, field.name);
		}
		else if ((m = match(field.subtype, /^ipv([46])$/)) != null) {
			let len = (m[1] == 4) ? 4 : 16;
			println(indent, "%s%s = arrtoip(buf.read('%dB'));", decl, field.name, len);
		}
		else if (field.size_ref < 0) {
			println(indent, "%s%s = buf.get('*');", decl, field.name);
			return true; // no more fields
		}
		else if (field.size_field && field.size < 0) {
			println(indent, "if (buf.pos() + %s - %d > end)", field.size_field.name, -field.size);
			println(indent, "	return null;");
			println(indent, "%s%s = buf.get(%s - %d);", decl, field.name, field.size_field.name, -field.size);
		}
		else if (field.size_field) {
			println(indent, "if (buf.pos() + %s > end)", field.size_field.name);
			println(indent, "	return null;");
			println(indent, "%s%s = buf.get(%s);", decl, field.name, field.size_field.name);
		}
		else {
			println(indent, "%s%s = buf.get(%d);", decl, field.name, field.size);
		}
	}
	else if (field.type === 'void') {
		// ignore
	}
	else {
		println(indent, "// %s%s = <%s / %s>;", decl, field.name, field.type, field.subtype);
	}

	if ('enum' in field) {
		println(indent, "if (!exists(defs.%s, %s))", uc(field.enum_name ?? field.name), field.name);
		println(indent, "	return null;");
	}
	else if ('min' in field || 'max' in field) {
		let smax = sprintf('%02x', field.max);
		let smin = sprintf('%0*x', length(field.max), field.min);
		let tmax = {
			'uint8_t': 0xff,
			'uint16_t': 0xffff,
			'uint32_t': 0xffffffff,
			'uint64_t': 0xffffffffffffffff
		}[field.type] || 0;

		if (field.min > 0 && (!tmax || field.max < tmax)) {
			println(indent, "if (%s < 0x%s || %s > 0x%s)", field.name, smin, field.name, smax);
			println(indent, "	return null;");
		}
		else if (!tmax || field.max < tmax) {
			println(indent, "if (%s > 0x%s)", field.name, smax);
			println(indent, "	return null;");
		}
		else if (field.min > 0) {
			println(indent, "if (%s < 0x%s)", field.name, smin);
			println(indent, "	return null;");
		}
	}

	return false;
}

let gen_decode_repeat;

function gen_decode_prop(indent, fields) {
	let bitfield_counter = '';

	for (let i = 0; i < length(fields); i++) {
		let field = fields[i];
		let optional = gen_require_condition(field.requires);
		let final = false;

		if (optional) {
			println(indent, "let %s = null;", field.name);
			println(indent, "if (%s) {", optional);
			indent++;
		}

		if (type(field.fields) === 'array') {
			gen_decode_repeat(indent, field, optional);
		}
		else if (field.size > 0 && field.size < 1) {
			let bitfield_members = [];
			let bitfield_length = 0.0;

			while (bitfield_length < 1 && i < length(fields)) {
				push(bitfield_members, fields[i]);
				bitfield_length += fields[i].size;
				i++;
			}

			gen_decode_bitfield(indent, bitfield_counter, bitfield_members);
			bitfield_counter = (bitfield_counter || 1) + 1;
			i--;
		}
		else {
			final = gen_decode_prop_lowlevel(indent, field, optional);
		}

		if (optional) {
			indent--;
			println(indent, "}");
		}

		if (final) return true;
	}

	return false;
}

gen_decode_repeat = function(indent, field, optional) {
	let itervar = 'f';
	for (let i = 0; i <= indent; i++) itervar = chr(ord(itervar) + 1);
	let decl = optional ? '' : 'const ';

	let fixed_length = 0;
	for (let subfield in field.fields) {
		if (!('repeat_ref' in subfield)) fixed_length += subfield.size;
	}

	if ('repeat_ref' in field) {
		if (field.repeat_field) {
			println(indent, "%s%s = [];", decl, field.name);
			println(indent, "for (let %s = 0; %s < %s; %s++) {", itervar, itervar, field.repeat_field.name, itervar);

			if (fixed_length) {
				println(indent, "	if (buf.pos() + %d > end)", fixed_length);
				println(indent, "		return null;");
			}
		}
		else if (fixed_length) {
			println(indent, "%s%s = [];", decl, field.name);
			println(indent, "while (buf.pos() + %d <= end) {", fixed_length);
		}
		else {
			println(indent, "%s%s = [];", decl, field.name);
			println(indent, "while (buf.pos() <= end) {");
		}

		indent++;
	}
	else {
		if (fixed_length) {
			println(indent, "if (buf.pos() + %d > end)", fixed_length);
			println(indent, "	return null;");
		}
	}

	gen_decode_prop(indent, field.fields);

	let assign_s = 'repeat_ref' in field ? "push(%s, \r" : "%s = \r";
	let assign_p = 'repeat_ref' in field ? ");" : ";";

	println(indent, assign_s, field.name);

	if (length(field.fields) > 1 || ('enum' in field.fields[0])) {
		gen_prop_assign(indent, field.fields);
	}
	else {
		println(0, "%s\r", field.fields[0].name);
	}

	println(0, assign_p);

	if ('repeat_ref' in field) {
		indent--;
		println(indent, "}");
	}
};

function gen_decode(indent, fields) {
	if (length(fields) === 0) {
		println(0, "(buf, end) => '',");
		return;
	}

	if (length(fields) === 1 && fields[0].size_ref === -1) {
		println(0, "(buf, end) => buf.get(end - buf.pos()),");
		return;
	}

	println(0, "(buf, end) => {");

	let fixed = 0;
	for (let field in fields) {
		if (!('fields' in field) && !('requires' in field)) {
			fixed += field.size;
		}
	}

	if (fixed > 0) {
		println(indent, "	if (buf.pos() + %d > end)", fixed);
		println(indent, "		return null;");
	}

	if (!gen_decode_prop(indent + 1, fields) && 0) {
		println(indent, "	if (buf.pos() < end)");
		println(indent, "		return null;");
	}

	println(indent, "	return \r");
	gen_prop_assign(indent + 1, fields);
	println(0, ";");
	println(indent, "};");
}

function gen_validate_prop(indent, val, field) {
	let limit = [null, '0xff', '0xffff', null, '0xffffffff', null, null, null, '0xffffffffffffffff'];
	let m;

	if (field.type === 'array') {
		if (field.repeat_field) {
			println(indent, 'if (type(%s%s) != "array" || length(%s%s) > %s)', val, field.name, val, field.name, limit[field.repeat_field.size]);
		}
		else {
			println(indent, 'if (type(%s%s) != "array")', val, field.name);
		}

		println(indent, '	return null;');
	}
	else if (field.type === 'void' || 'ref' in field) {
		return;
	}
	else if (field.subtype === 'mac') {
		let var_name = (val ? '' : '_') + field.name;

		if ('requires' in field) {
			println(indent, 'let %s = null;', var_name);
			println(indent, 'if (%s%s != null) {', val, field.name);
			println(indent, '	%s = hexdec(match(%s%s, /^[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}$/i)?.[0], ":");', var_name, val, field.name);
			println(indent, '	if (%s == null)', var_name);
			println(indent, '		return null;');
			println(indent, '}');
		}
		else {
			println(indent, 'const %s = hexdec(match(%s%s, /^[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}:[0-9a-f]{2}$/i)?.[0], ":");', var_name, val, field.name);
			println(indent, 'if (%s == null)', var_name);
			println(indent, '	return null;');
		}
	}
	else if ((m = match(field.subtype, /^ipv([46])$/)) != null) {
		let var_name = (val ? '' : '_') + field.name;
		let len = (m[1] == 4) ? 4 : 16;

		if ('requires' in field) {
			println(indent, 'let %s = null;', var_name);
			println(indent, 'if (%s%s != null) {', val, field.name);
			println(indent, '	%s = iptoarr(%s%s);', var_name, val, field.name);
			println(indent, '	if (length(%s) != %d)', var_name, len);
			println(indent, '		return null;');
			println(indent, '}');
		}
		else {
			println(indent, 'const %s = iptoarr(%s%s);', var_name, val, field.name);
			println(indent, 'if (length(%s) != %d)', var_name, len);
			println(indent, '	return null;');
		}
	}
	else if ('min' in field && 'max' in field) {
		println(indent, 'if (type(%s%s) != "int" || %s%s < %u || %s%s > %u)', val, field.name, val, field.name, field.min, val, field.name, field.max);
		println(indent, '	return null;');
	}
	else if ('enum' in field) {
		println(indent, 'if (!(%s%s in [ %s ]))', val, field.name, join(', ', sort(keys(field.enum))));
		println(indent, '	return null;');
	}
	else if ((m = match(field.type, /^uint(8|16|32|64)_t$/)) != null) {
		println(indent, 'if (type(%s%s) != "int" || %s%s < 0 || %s%s > %s)', val, field.name, val, field.name, val, field.name, limit[m[1] / 8]);
		println(indent, '	return null;');
	}
	else if (field.subtype === 'bitfield') {
		let max = (1 << (field.size / 0.125)) - 1;
		if (max === 1) {
			println(indent, 'if (type(%s%s) != "bool")', val, field.name);
		}
		else {
			println(indent, 'if (type(%s%s) != "int" || %s%s < 0 || %s%s > 0b%s)', val, field.name, val, field.name, val, field.name, bitstr(max));
		}
		println(indent, '	return null;');
	}
	else if (field.type === 'string') {
		let o1 = '', o2 = '';

		if ('requires' in field)
			o1 = sprintf('%s%s != null && (', val, field.name),  o2 = ')';

		if (field.size_field && field.size < 0) {
			println(indent, 'if (%stype(%s%s) != "string" || length(%s%s) > %s - %u%s)', o1, val, field.name, val, field.name, limit[field.size_field.size], field.size, o2);
		}
		else if (field.size_field) {
			let sz = field.size_field.size >= 1 ? limit[field.size_field.size] : 2 ** (field.size_field.size / 0.125);
			println(indent, 'if (%stype(%s%s) != "string" || length(%s%s) > %s%s)', o1, val, field.name, val, field.name, sz, o2);
		}
		else if (field.size > 0) {
			println(indent, 'if (%stype(%s%s) != "string" || length(%s%s) > %u%s)', o1, val, field.name, val, field.name, field.size, o2);
		}
		else {
			println(indent, 'if (%stype(%s%s) != "string"%s)', o1, val, field.name, o2);
		}

		println(indent, '	return null;');
	}
}

function size_expr(prefix, ref_fields) {
	let exprs = [];

	let fields = type(ref_fields) === 'array' ? ref_fields : [ref_fields];
	for (let field in fields) {
		if (field.type === 'array' && field.subtype !== 'array') {
			assert(field.subtype == 'uint8_t', 'Unexpected array subtype');
			push(exprs, sprintf('length(%s%s)', prefix, field.name));
		}
		else if (field.type === 'array' && !('repeat_ref' in field)) {
			let fixed_length = 0;

			for (let subfield in field.fields) {
				if (!('repeat_ref' in subfield)) fixed_length += subfield.size;
			}

			push(exprs, sprintf('(%s%s != null ? %d : null)', prefix, field.name, fixed_length));
		}
		else {
			push(exprs, sprintf('length(%s%s)', prefix, field.name));
		}
	}

	return join(' ?? ', exprs);
}

let gen_encode_prop;

function value_ref(prefix, field) {
	if (match(field.subtype, /^(mac|ipv4|ipv6)$/)) {
		return (prefix ? '' : '_') + field.name;
	}

	return prefix + field.name;
}

function gen_default_prop(indent, val, field) {
	if (type(field.requires) === 'object') {
		let ref = value_ref(val, field);

		for (let ref_id in sort(keys(field.requires))) {
			let req_field = field.requires[ref_id].field;
			let values = field.requires[ref_id].values;

			let req_ref = value_ref(val, req_field);

			println(indent, '%s ??= (%s != null) && %d;', req_ref, ref, values[0]);
		}
	}
}

function gen_encode_prop_lowlevel(indent, val, field, depth, bitoff) {
	let pack_fmt = [null, 'B', '!H', null, '!L', null, null, null, '!Q'];
	let limit = [null, '0xff', '0xffff', null, '0xffffffff', null, null, null, '0xffffffffffffffff'];
	let m;

	if (type(field.fields) === 'array') {
		let subfields = filter(field.fields, f => f.type !== 'void' && !('ref' in f));

		if (length(subfields) === 1) {
			println(indent, 'for (let %s in %s%s) {', subfields[0].name, val, field.name);
			gen_validate_prop(indent + 1, '', subfields[0]);
			gen_default_prop(indent + 1, '', subfields[0]);

			let off = 0;
			for (let subfield in field.fields) {
				gen_encode_prop(indent + 1, '', subfield, (depth || 1) + 1, off % 8);
				off += (subfield.size || 0) * 8;
			}

			println(indent, '}');
		}
		else {
			let itemvar = sprintf('item%s', depth || '');

			println(indent, 'for (let %s in %s%s) {', itemvar, val, field.name);
			println(indent, '	if (type(%s) != "object")', itemvar);
			println(indent, '		return null;');

			for (let subfield in field.fields) {
				gen_validate_prop(indent + 1, itemvar + ".", subfield);
			}

			println(0, '');

			for (let subfield in field.fields) {
				gen_default_prop(indent + 1, itemvar + ".", subfield);
			}

			println(0, '');

			let off = 0;
			for (let subfield in field.fields) {
				gen_encode_prop(indent + 1, itemvar + ".", subfield, (depth || 1) + 1, off % 8);
				off += (subfield.size || 0) * 8;
			}

			println(indent, '}');
		}
	}
	else if (field.type === 'array') {
		assert(field.subtype == 'uint8_t', 'Unexpected array subtype');

		let itemvar = sprintf('item%s', depth || '');

		println(indent, 'for (let %s in %s%s) {', itemvar, val, field.name);
		gen_validate_prop(indent + 1, '', { name: itemvar, type: field.subtype });
		println(indent, "	buf.put('B', %s);", itemvar);
		println(indent, '}');
	}
	else if (field.size > 0 && field.size < 1) {
		let num = (field.size / 0.125);

		if (bitoff == null || bitoff == 0) {
			println(indent, "buf.put('B', 0");
		}

		let ref = val + field.name;

		if (field.value_field) {
			ref = size_expr(val, field.value_field);
		}

		if (field.type !== 'void') {
			if (num === 1.0) {
				println(indent, '	| (%s << %u)', ref, 7 - bitoff);
			}
			else {
				println(indent, '	| ((%s & 0b%s) << %u)', ref, bitstr((1 << num) - 1), 7 - bitoff - num + 1);
			}
		}

		if (bitoff + num === 8.0) {
			println(indent, ');');
		}
	}
	else if (field.type in ['number', 'boolean'] && match(field.subtype, /^(uint8_t|uint16_t|uint32_t|uint64_t|boolean)$/)) {
		let ref = val + field.name;
		let ref_field = field.value_field || field.array_field;
		const fmt = (field.type === 'boolean') ? '?' : pack_fmt[field.size];

		if (ref_field) {
			ref = size_expr(val, ref_field);
		}

		println(indent, "buf.put('%s', %s);", fmt, ref);
	}
	else if (field.value_field) {
		let fmt = pack_fmt[field.size] || sprintf('%us', field.size);
		println(indent, "buf.put('%s', %s);", fmt, size_expr(val, field.value_field));
	}
	else if (field.array_field) {
		let fmt = pack_fmt[field.size] || sprintf('%us', field.size);
		println(indent, "buf.put('%s', %s);", fmt, size_expr(val, field.array_field));
	}
	else if (field.subtype === 'mac') {
		let var_name = (val ? '' : '_') + field.name;
		println(indent, "buf.put('6s', %s);", var_name);
	}
	else if ((m = match(field.subtype, /^ipv([46])$/)) != null) {
		let var_name = (val ? '' : '_') + field.name;
		let len = (m[1] == 4) ? 4 : 16;
		println(indent, "buf.put('%dB', ...%s);", len, var_name);
	}
	else if (field.type === 'string') {
		if (field.size === '*' || field.size <= 0)
			println(indent, "buf.put('*', %s%s);", val, field.name);
		else
			println(indent, "buf.put('%us', %s%s);", field.size, val, field.name);
	}
}

gen_encode_prop = function(indent, val, field, depth, bitoff) {
	if (type(field.requires) === 'object') {
		let ref = value_ref(val, field);

		println(indent, 'if (%s != null)', ref);
		gen_encode_prop_lowlevel(indent + 1, val, field, depth, bitoff);
	}
	else {
		gen_encode_prop_lowlevel(indent, val, field, depth, bitoff);
	}
};

function gen_encode(indent, fields) {
	let fields_filtered = filter(fields, f => f.type !== 'void' && !('ref' in f));

	if (length(fields_filtered) === 0) {
		println(0, '(buf) => buf,');
		return;
	}

	if (length(fields) === 1 && 'size_ref' in fields[0] && fields[0].size_ref === -1) {
		println(0, "(buf, payload) => buf.put('*', payload),");
		return;
	}

	println(0, '(buf, %s) => {', length(fields_filtered) > 1 ? 'tlv' : fields_filtered[0].name);

	if (length(fields_filtered) === 1) {
		gen_validate_prop(indent + 1, '', fields_filtered[0]);
		println(0, '');
		gen_default_prop(indent + 1, '', fields_filtered[0]);
		println(0, '');
		let off = 0;
		for (let field in fields) {
			gen_encode_prop(indent + 1, '', field, null, off % 8);
			off += (field.size || 0) * 8;
		}
	}
	else {
		println(indent, '	if (type(tlv) != "object")');
		println(indent, '		return null;');

		for (let field in fields_filtered) {
			gen_validate_prop(indent + 1, 'tlv.', field);
		}

		println(0, '');

		for (let field in fields_filtered) {
			gen_default_prop(indent + 1, 'tlv.', field);
		}

		println(0, '');

		let off = 0;
		for (let field in fields) {
			gen_encode_prop(indent + 1, 'tlv.', field, null, off % 8);
			off += (field.size || 0) * 8;
		}
	}

	println(0, '');
	println(indent, '	return buf;');
	println(indent, '};');
}

function add_enums(enums, fields) {
	for (let field in fields) {
		if ('fields' in field) {
			add_enums(enums, field.fields);
		}

		if ('enum' in field) {
			const name = uc(field.enum_name ?? field.name);

			if (exists(enums, name))
				warn(`Enum ${name} redefined\n`);

			enums[name] = field.enum;
		}
	}
}

function gen_enums(indent, fields) {
	let enums = {};

	add_enums(enums, fields);

	for (let enum_name in sort(keys(enums))) {
		println(indent, "%s: {", enum_name);

		for (let key in sort(keys(enums[enum_name]))) {
			println(indent, "	[%s]: '%s',", key, enums[enum_name][key]);
		}

		println(indent, "},");
		println(0, "");
	}
}

function fixup_refs(fields) {
	for (let member in fields) {
		if (type(member.fields) === 'array') {
			member.type = 'array';
			member.subtype = 'array';

			if ('repeat_ref' in member && member.repeat_ref >= 0) {
				let repeat_field = filter(fields, f => f.id == member.repeat_ref)[0];

				if (repeat_field) {
					member.repeat_field = repeat_field;

					if (type(repeat_field.array_field) === 'array') {
						push(repeat_field.array_field, member);
					}
					else if ('array_field' in repeat_field) {
						repeat_field.array_field = [ repeat_field.array_field, member ];
					}
					else {
						repeat_field.array_field = member;
					}
				}
				else {
					die("Array field " + member.name + " references not existing field " + member.repeat_ref + "\n");
				}
			}
			else if ('size_ref' in member && member.size_ref >= 0) {
				let size_field = filter(fields, f => f.id == member.size_ref)[0];

				if (size_field) {
					member.size_field = size_field;

					if (type(size_field.value_field) === 'array') {
						push(size_field.value_field, member);
					}
					else if ('value_field' in size_field) {
						size_field.value_field = [ size_field.value_field, member ];
					}
					else {
						size_field.value_field = member;
					}
				}
				else {
					die("Array field " + member.name + " references not existing field " + member.size_ref + "\n");
				}
			}

			fixup_refs(member.fields);
		}
		else if ('size_ref' in member && member.size_ref >= 0) {
			let size_field = filter(fields, f => f.id == member.size_ref)[0];

			if (size_field) {
				member.size_field = size_field;

				if (type(size_field.value_field) === 'array') {
					push(size_field.value_field, member);
				}
				else if ('value_field' in size_field) {
					size_field.value_field = [ size_field.value_field, member ];
				}
				else {
					size_field.value_field = member;
				}
			}
			else {
				die("String field " + member.name + " references not existing field " + member.size_ref + "\n");
			}
		}

		if (type(member.requires) === 'object') {
			for (let ref_id in sort(keys(member.requires))) {
				if (type(member.requires[ref_id]) == 'object') die(member.requires[ref_id]);
				let req_field = filter(fields, f => f.id == +ref_id)[0];

				if (req_field) {
					let values = type(member.requires[ref_id]) === 'array'
						? member.requires[ref_id]
						: [ member.requires[ref_id] ];

					member.requires[ref_id] = { field: req_field, values: values };
				}
				else {
					die("Field " + member.name + " references not existing depending field " + ref_id + "\n");
				}
			}
		}
	}

	return fields;
}

function gen_cmdu(indent, cmdu) {
	println(0, "{");

	//let keys = ['type', 'name', 'description', 'tlvs', 'transmission_type', 'relay_indicator', 'protocol', 'standards'];
	let keys = ['transmission_type', 'relay_indicator', 'protocol', 'standards', 'tlvs'];
	for (let key in keys) {
		if (key === 'type') {
			println(indent, "	type: %d,", cmdu.type);
		}
		else if (key === 'relay_indicator') {
			println(indent, "	relay_indicator: %s,", cmdu.relay_indicator ? 'true' : 'false');
		}
		else if (key === 'tlvs') {
			if (length(cmdu.tlvs) > 0) {
				println(indent, "	tlvs: [");

				for (let spec in cmdu.tlvs) {
					println(indent, "		[ %d, '%s', 0x%02x ],",
						spec.profile || 0,
						spec.quantity,
						int(spec.type, 16));
				}

				println(indent, "	],");
			}
		}
		else if (key === 'standards') {
			println(indent, "	standards: [ '%s' ],", join("', '", cmdu.standards));
		}
		else if (match(cmdu[key], /'/) != null) {
			println(indent, '	%s: "%s",', key, cmdu[key]);
		}
		else {
			println(indent, "	%s: '%s',", key, cmdu[key]);
		}
	}

	println(indent, "}\r");
}

function gen_codecs(state) {
	output = '';

	println(0, "import defs from 'umap.defs';");
	println(0, "");

	for (let i, mode in [ [ 'encoder', gen_encode ], [ 'decoder', gen_decode ], [ 'extended_encoder', gen_encode ], [ 'extended_decoder', gen_decode ] ]) {
		if (i > 0)
			println(0, "");

		println(0, "// -----------------------------------------------------------------------------");
		println(0, "// TLV %s ROUTINES", replace(uc(mode[0]), '_', ' '));
		println(0, "// -----------------------------------------------------------------------------");
		println(0, "");
		println(0, "export const %s = [];", mode[0]);

		for (let tlv_type in state.tlv) {
			let tlv = state.tlv[tlv_type];
			let ext = ('subtype' in tlv);
			let id = ext ? sprintf('0x%04x', tlv.subtype) : sprintf('0x%02x', tlv.type);

			if (i == 0)
				fixup_refs(tlv.fields);

			if (!ext ^ !index(mode[0], 'extended_')) {
				println(0, "");
				println(0, "// %s - %s", id, tlv.name);
				println(0, "// %s", tlv.standard);
				println(0, "%s[%s] = \r", mode[0], id);

				mode[1](0, tlv.fields);
			}
		}
	}

	return output;
}

function gen_definitions(state) {
	output = '';

	println(0, "export default {");
	println(0, "	IEEE1905_MULTICAST_MAC: '01:80:c2:00:00:13',");
	println(0, "	LLDP_NEAREST_BRIDGE_MAC: '01:80:c2:00:00:0e',");
	println(0, "");
	println(0, "	CMDU_F_LASTFRAG: 0b10000000,");
	println(0, "	CMDU_F_ISRELAY: 0b01000000,");
	println(0, "");

	for (let key in sort(keys(state.cmdu))) {
		let type = state.cmdu[key].type;
		let name = uc(state.cmdu[key].name);

		name = replace(name, /\b1905(\.1)?\b/g, "IEEE1905");
		name = replace(name, /\bWI-FI 6\b/g, "WIFI6");
		name = replace(name, /[^A-Z0-9_]+/g, "_");

		println(0, "	MSG_%s: 0x%04x,", name, type);
	}

	println(0, "");

	for (let key in sort(keys(state.tlv))) {
		let type = state.tlv[key].type;
		let name = uc(state.tlv[key].name);

		name = replace(name, /\b1905(\.1)?\b/g, "IEEE1905");
		name = replace(name, /\bWI-FI 6\b/g, "WIFI6");
		name = replace(name, /[^A-Z0-9_]+/g, "_");

		if (type !== 222) {
			println(0, "	TLV_%s: 0x%04x,", name, type);
		}
		else {
			let subtype = state.tlv[key].subtype;

			if (subtype === 1) {
				println(0, "");
				println(0, "	TLV_EXTENDED: 0xde,");
			}

			println(0, "	TLV_EXTENDED_%s: 0x%04x,", name, subtype);
		}
	}

	println(0, "");

	for (let enum_name in sort(keys(state.enums))) {
		println(0, "	%s: {", enum_name);

		for (let key in sort(keys(state.enums[enum_name]))) {
			println(0, "		[%s]: '%s',", key, state.enums[enum_name][key]);
		}

		println(0, "	},");
		println(0, "");
	}

	println(0, "};");

	return output;
}

function gen_policies(state) {
	output = '';

	println(0, "export policy = {");

	for (let cmdu_type in sort(keys(state.cmdu))) {
		println(0, "	// 0x%04x - %s", cmdu_type, state.cmdu[cmdu_type].name);
		println(0, "	// %s", state.cmdu[cmdu_type].description);
		println(0, "	[0x%04x]: \r", cmdu_type);
		gen_cmdu(1, state.cmdu[cmdu_type]);
		println(0, ",");
		println(0, "");
	}

	println(0, "};");

	return output;
}

function main(args) {
	if (length(args) < 3) {
		print("Usage: generator.uc <output dir> <input spec...>\n");
		exit(1);
	}

	let output_dir = shift(args);
	let state = { cmdu: {}, tlv: {}, enums: {} };

	for (let file in args) {
		let data = json(open(file, 'r'));

		if ('cmdu' in data) {
			for (let cmdu_type in sort(keys(data.cmdu))) {
				let cmdu = state.cmdu[cmdu_type] ??= {
					standards: [],
					tlvs: []
				};

				if ('type' in cmdu && cmdu.type !== data.cmdu[cmdu_type].type) {
					warn(sprintf("Expected CMDU type mismatch: %d (%s) vs. %d (%s)\n",
						cmdu.type, cmdu.name ?? '?',
						data.cmdu[cmdu_type].type, data.cmdu[cmdu_type].name ?? '?'));
					continue;
				}

				// properties to copy
				for (let prop in ['type', 'name', 'description']) {
					cmdu[prop] ??= data.cmdu[cmdu_type][prop];
					cmdu[prop] ??= data.cmdu[cmdu_type][prop];
				}

				// properties that should match
				for (let prop in ['protocol', 'relay_indicator', 'transmission_type']) {
					if (prop in cmdu && cmdu[prop] !== data.cmdu[cmdu_type][prop]) {
						warn(sprintf("Expected CMDU type %d property %s value %s, got %s instead\n",
							cmdu.type, prop, cmdu[prop], data.cmdu[cmdu_type][prop]));
					}

					cmdu[prop] = data.cmdu[cmdu_type][prop];
				}

				// merge protocols
				push(cmdu.standards, data.cmdu[cmdu_type].standard);

				// merge TLVs
				for (let tlv in data.cmdu[cmdu_type].tlvs) {
					let tlvs = filter(cmdu.tlvs, t => t.type !== tlv.type);
					if (length(tlvs) !== length(cmdu.tlvs)) {
						warn(sprintf("CMDU type %d TLV %s is overridden by definition from %s\n",
							cmdu.type, tlv.type, file));
					}

					push(cmdu.tlvs, tlv);
				}
			}
		}

		if ('tlv' in data) {
			for (let tlv_type in sort(keys(data.tlv))) {
				if (tlv_type in state.tlv) {
					warn(sprintf("TLV type %s is overridden by definition from %s\n",
						tlv_type, file));
				}

				state.tlv[tlv_type] = data.tlv[tlv_type];

				add_enums(state.enums, data.tlv[tlv_type].fields);
			}
		}
	}

	// Generate codecs
	let codec_path = sprintf('%s/codec.uc', output_dir);
	writefile(codec_path, gen_codecs(state));

	// Generate definitions
	let defs_path = sprintf('%s/defs.uc', output_dir);
	writefile(defs_path, gen_definitions(state));

	// Generate policies
	let policy_path = sprintf('%s/policies.uc', output_dir);
	writefile(policy_path, gen_policies(state));
}

main(ARGV);
