// Check Lisp-produced PNG bytes independently of ZPNG and PNG-READ.
import assert from 'node:assert/strict';
import {inflateSync} from 'node:zlib';
import {createHash} from 'node:crypto';
import {launchChrome} from './chrome.mjs';

function crc32(bytes) {
  let crc=0xffffffff;
  for(const byte of bytes){crc^=byte;for(let bit=0;bit<8;bit++)crc=(crc>>>1)^((crc&1)?0xedb88320:0);}
  return (crc^0xffffffff)>>>0;
}

export async function verifyPngOutput(value) {
  const [checks,images]=JSON.parse(JSON.parse(value.slice(1,-1)));
  assert.equal(checks,44);
  assert.deepEqual(images.map(image=>image[0]),
    ['rgba','default','rgba-repeat','palette','palette-mutation','tile','scaled','multirow']);
  const chrome=await launchChrome(),results=[];
  try {
    const page=await chrome.page('about:blank');
    for(const [name,width,height,octets,expected] of images){
      const bytes=Buffer.from(octets),chunks=[],compressed=[];
      assert.deepEqual([...bytes.subarray(0,8)],[137,80,78,71,13,10,26,10]);
      let offset=8;
      while(offset<bytes.length){
        const length=bytes.readUInt32BE(offset),type=bytes.toString('ascii',offset+4,offset+8);
        const end=offset+8+length;
        assert.ok(end+4<=bytes.length);
        assert.equal(bytes.readUInt32BE(end),crc32(bytes.subarray(offset+4,end)),`${name} ${type} CRC`);
        const data=bytes.subarray(offset+8,end);chunks.push(type);
        if(type==='IHDR'){
          assert.equal(data.readUInt32BE(0),width);assert.equal(data.readUInt32BE(4),height);
          assert.deepEqual([...data.subarray(8)],[8,6,0,0,0]);
        }
        if(type==='IDAT')compressed.push(data);
        offset=end+4;
      }
      assert.equal(chunks[0],'IHDR');assert.equal(chunks.at(-1),'IEND');
      const raw=inflateSync(Buffer.concat(compressed)),stride=width*4;
      assert.equal(raw.length,height*(stride+1));assert.equal(expected.length,width*height);
      // ZPNG's unfiltered RGBA rows preserve even hidden RGB at alpha zero.
      for(let y=0;y<height;y++){
        assert.equal(raw[y*(stride+1)],0);
        assert.deepEqual([...raw.subarray(y*(stride+1)+1,(y+1)*(stride+1))],
          expected.slice(y*width,(y+1)*width).flat(),`${name} row ${y}`);
      }
      const decoded=await page.evaluate(`(async()=>{
        const bytes=Uint8Array.from(atob(${JSON.stringify(bytes.toString('base64'))}),c=>c.charCodeAt(0));
        const bitmap=await createImageBitmap(new Blob([bytes],{type:'image/png'}));
        const canvas=new OffscreenCanvas(bitmap.width,bitmap.height),ctx=canvas.getContext('2d');
        ctx.fillStyle='white';ctx.fillRect(0,0,canvas.width,canvas.height);ctx.drawImage(bitmap,0,0);
        const result={width:canvas.width,height:canvas.height,pixels:Array.from(ctx.getImageData(0,0,canvas.width,canvas.height).data)};
        bitmap.close();return result;})()`);
      assert.equal(decoded.width,width);assert.equal(decoded.height,height);
      expected.forEach(([r,g,b,a],i)=>{
        [r,g,b].forEach((v,c)=>assert.ok(Math.abs(decoded.pixels[4*i+c]-Math.round((v*a+255*(255-a))/255))<=1,`${name} pixel ${i} channel ${c}`));
        assert.equal(decoded.pixels[4*i+3],255);
      });
      results.push({name,width,height,bytes:bytes.length,sha256:createHash('sha256').update(bytes).digest('hex'),chunks});
    }
  } finally {await chrome.close();}
  return {status:'passed',checks,images:results,validation:'PNG structure, CRC, independent zlib decompression and exact RGBA rows, plus native Chrome decoding over white'};
}
