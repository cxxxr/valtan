// Independent small PNG encoder for test data, following PNG's sample packing,
// filters and Adam7 pass order: https://www.w3.org/TR/png-3/
import {deflateSync} from 'node:zlib';

function crc32(bytes){
  let crc=0xffffffff;
  for(const byte of bytes){crc^=byte;for(let bit=0;bit<8;bit++)crc=(crc>>>1)^((crc&1)?0xedb88320:0);}
  return (crc^0xffffffff)>>>0;
}
function chunk(type,data){
  const body=Buffer.concat([Buffer.from(type),data]),header=Buffer.alloc(4),crc=Buffer.alloc(4);
  header.writeUInt32BE(data.length);crc.writeUInt32BE(crc32(body));
  return Buffer.concat([header,body,crc]);
}
const word=value=>{const bytes=Buffer.alloc(2);bytes.writeUInt16BE(value);return bytes;};
function pack(samples,bits){
  const data=Buffer.alloc(Math.ceil(samples.length*bits/8));
  samples.forEach((value,i)=>{
    if(bits===16)data.writeUInt16BE(value,i*2);
    else if(bits===8)data[i]=value;
    else data[Math.floor(i*bits/8)]|=value<<(8-bits-(i*bits)%8);
  });return data;
}
function paeth(a,b,c){
  const p=a+b-c,pa=Math.abs(p-a),pb=Math.abs(p-b),pc=Math.abs(p-c);
  return pa<=pb && pa<=pc?a:pb<=pc?b:c;
}
export function pngFixtures(){
  const result=[];
  for(const [type,depths,channels] of [[0,[1,2,4,8,16],1],[2,[8,16],3],
    [3,[1,2,4,8],1],[4,[8,16],2],[6,[8,16],4]]){
    for(const bits of depths)for(const interlace of [0,1]){
      const width=5,height=3,max=2**bits-1;
      // Duplicate red entries have different alpha: recovering palette indices
      // from expanded RGB values would silently lose this distinction.
      const palette=[[255,0,0],[0,255,0],[0,0,255],[255,0,0]].slice(0,Math.min(4,2**bits));
      const transparency=type===3?Buffer.from([0,128]):type===0?word(0):
        type===2?Buffer.concat([word(0),word(0),word(0)]):null;
      const sample=(x,y)=>Array.from({length:channels},(_,c)=>{
        if(type===3)return (x+y)%palette.length;
        if(x===0 && y===0)return 0;
        if(x===4 && y===2)return max;
        return ((x+2*y+3*c)*Math.max(1,Math.floor(max/7)))%(max+1);
      });
      const rgba=[];
      const scale=x=>Math.round(x*255/max);
      for(let y=0;y<height;y++)for(let x=0;x<width;x++){
        const s=sample(x,y);let pixel;
        if(type===0)pixel=[...Array(3).fill(scale(s[0])),s[0]===0?0:255];
        if(type===2)pixel=[...s.map(scale),s.every(v=>v===0)?0:255];
        if(type===3)pixel=[...palette[s[0]],transparency[s[0]]??255];
        if(type===4)pixel=[...Array(3).fill(scale(s[0])),scale(s[1])];
        if(type===6)pixel=s.map(scale);
        rgba.push(pixel);
      }
      const passes=interlace?[[0,0,8,8],[4,0,8,8],[0,4,4,8],[2,0,4,4],[0,2,2,4],[1,0,2,2],[0,1,1,2]]:[[0,0,1,1]];
      const scanlines=[];let line=0;
      for(const [sx,sy,dx,dy] of passes){
        if(sx>=width || sy>=height)continue;
        let previous=[];const stride=Math.max(1,Math.ceil(bits*channels/8));
        for(let y=sy;y<height;y+=dy){
          const samples=[];for(let x=sx;x<width;x+=dx)samples.push(...sample(x,y));
          const row=pack(samples,bits),filter=(line++)%5,filtered=Buffer.alloc(row.length);
          for(let i=0;i<row.length;i++){
            const a=row[i-stride]??0,b=previous[i]??0,c=previous[i-stride]??0;
            const predictor=[0,a,b,Math.floor((a+b)/2),paeth(a,b,c)][filter];
            filtered[i]=(row[i]-predictor)&255;
          }
          scanlines.push(Buffer.from([filter]),filtered);previous=row;
        }
      }
      const header=Buffer.alloc(13);header.writeUInt32BE(width);header.writeUInt32BE(height,4);
      header[8]=bits;header[9]=type;header[12]=interlace;
      const chunks=[chunk('IHDR',header)];
      if(type===3)chunks.push(chunk('PLTE',Buffer.from(palette.flat())));
      if(transparency)chunks.push(chunk('tRNS',transparency));
      chunks.push(chunk('IDAT',deflateSync(Buffer.concat(scanlines))),chunk('IEND',Buffer.alloc(0)));
      const extension=type===0 && bits===1 && !interlace?'PNG':'png';
      result.push({name:`color-${type}-depth-${bits}-interlace-${interlace}.${extension}`,width,height,rgba,
        bytes:Buffer.concat([Buffer.from([137,80,78,71,13,10,26,10]),...chunks])});
    }
  }
  return result;
}
