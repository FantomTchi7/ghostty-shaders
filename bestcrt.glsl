// In-game CRT shader
// Author: sarphiv
// License: CC BY-NC-SA 4.0
// Description:
//   Shader for ghostty that is focussed on being usable while looking like a stylized CRT terminal in a modern video game.
//   I know a tiny bit about shaders, and nothing about GLSL,
//   so this is a Frakenstein's monster combination of other shaders together with a lot of surgery.
//   On the bright side, i've cleaned up the body parts and surgery a lot.

// Based on:
//   1. https://gist.github.com/mitchellh/39d62186910dcc27cad097fed16eb882 (forces the choice of license)
//   2. https://gist.github.com/qwerasd205/c3da6c610c8ffe17d6d2d3cc7068f17f
//   3. https://gist.github.com/seanwcom/0fbe6b270aaa5f28823e053d3dbb14ca


// Settings:
// Barrel distortion strength - Higher values = more distortion
#define CURVE 0.2

// Distorted TV effects
// Change these values to 0.0 to turn off individual effects
float vertJerkOpt = 0.0;
float vertMovementOpt = 0.0;
float bottomStaticOpt = 0.5;
float scalinesOpt = 0.1;
float rgbOffsetOpt = 0.1;
float horzFuzzOpt = 0.0;

// How much of the non-linearly darkened colors are mixed in
// [0, 1]
#define DARKEN_MIX 0.4

// How far in the vignette spreads
// x \in R : x >= 0
#define VIGNETTE_SPREAD 0.3
// How bright the vignette is
// x \in R : x >= 0
#define VIGNETTE_BRIGHTNESS 6.4

// Tint all colors
// [0, 1]^3
#define TINT 0.93, 1.00, 0.96

// How visible the flicker effect is
// x \in R : x >= 0
#define FLICKER_STRENGTH 0.05
// How fast the screen flickers
// x \in R : x > 0
#define FLICKER_FREQUENCY 15.0

// How big the bloom is
// x \in R : x >= 0
#define BLOOM_SPREAD 8.0
// How visible the bloom is
// [0, 1]
#define BLOOM_STRENGTH 0.04

// How fast colors fade in and out
// [0, 1]
#define FADE_FACTOR 0.55



// Disabled values for when the settings are not defined
#ifndef DARKEN_MIX
#define DARKEN_MIX 0.0
#endif

#if !defined(VIGNETTE_SPREAD) || !defined(VIGNETTE_BRIGHTNESS)
#undef VIGNETTE_SPREAD
#undef VIGNETTE_BRIGHTNESS
#define VIGNETTE_SPREAD 0.0
#define VIGNETTE_BRIGHTNESS 1.0
#endif

#ifndef TINT
#define TINT 1.00, 1.00, 1.00
#endif

#if !defined(FLICKER_STRENGTH) || !defined(FLICKER_FREQUENCY)
#undef FLICKER_STRENGTH
#undef FLICKER_FREQUENCY
#define FLICKER_STRENGTH 0.0
#define FLICKER_FREQUENCY 1.0
#endif

#if !defined(BLOOM_SPREAD) || !defined(BLOOM_STRENGTH)
#undef BLOOM_SPREAD
#undef BLOOM_STRENGTH
#define BLOOM_SPREAD 0.0
#define BLOOM_STRENGTH 0.0
#endif

#ifndef FADE_FACTOR
#define FADE_FACTOR 1.00
#endif



// Constants
#define PI 3.1415926535897932384626433832795

#ifdef BLOOM_SPREAD
// Golden spiral samples used for bloom.
//   [x, y, weight] weight is inverse of distance.
const vec3[24] bloom_samples = {
    vec3( 0.1693761725038636,  0.9855514761735895,  1),
    vec3(-1.333070830962943,   0.4721463328627773,  0.7071067811865475),
    vec3(-0.8464394909806497, -1.51113870578065,    0.5773502691896258),
    vec3( 1.554155680728463,  -1.2588090085709776,  0.5),
    vec3( 1.681364377589461,   1.4741145918052656,  0.4472135954999579),
    vec3(-1.2795157692199817,  2.088741103228784,   0.4082482904638631),
    vec3(-2.4575847530631187, -0.9799373355024756,  0.3779644730092272),
    vec3( 0.5874641440200847, -2.7667464429345077,  0.35355339059327373),
    vec3( 2.997715703369726,   0.11704939884745152, 0.3333333333333333),
    vec3( 0.41360842451688395, 3.1351121305574803,  0.31622776601683794),
    vec3(-3.167149933769243,   0.9844599011770256,  0.30151134457776363),
    vec3(-1.5736713846521535, -3.0860263079123245,  0.2886751345948129),
    vec3( 2.888202648340422,  -2.1583061557896213,  0.2773500981126146),
    vec3( 2.7150778983300325,  2.5745586041105715,  0.2672612419124244),
    vec3(-2.1504069972377464,  3.2211410627650165,  0.2581988897471611),
    vec3(-3.6548858794907493, -1.6253643308191343,  0.25),
    vec3( 1.0130775986052671, -3.9967078676335834,  0.24253562503633297),
    vec3( 4.229723673607257,   0.33081361055181563, 0.23570226039551587),
    vec3( 0.40107790291173834, 4.340407413572593,   0.22941573387056174),
    vec3(-4.319124570236028,   1.159811599693438,   0.22360679774997896),
    vec3(-1.9209044802827355, -4.160543952132907,   0.2182178902359924),
    vec3( 3.8639122286635708, -2.6589814382925123,  0.21320071635561041),
    vec3( 3.3486228404946234,  3.4331800232609,     0.20851441405707477),
    vec3(-2.8769733643574344,  3.9652268864187157,  0.20412414523193154)
};
#endif

// Noise generation functions borrowed from: 
// https://github.com/ashima/webgl-noise/blob/master/src/noise2D.glsl
vec3 mod289(vec3 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec2 mod289(vec2 x) {
  return x - floor(x * (1.0 / 289.0)) * 289.0;
}

vec3 permute(vec3 x) {
  return mod289(((x*34.0)+1.0)*x);
}

float snoise(vec2 v) {
  const vec4 C = vec4(0.211324865405187,  // (3.0-sqrt(3.0))/6.0
                      0.366025403784439,  // 0.5*(sqrt(3.0)-1.0)
                     -0.577350269189626,  // -1.0 + 2.0 * C.x
                      0.024390243902439); // 1.0 / 41.0
// First corner
  vec2 i  = floor(v + dot(v, C.yy) );
  vec2 x0 = v -   i + dot(i, C.xx);
// Other corners
  vec2 i1;
  i1 = (x0.x > x0.y) ? vec2(1.0, 0.0) : vec2(0.0, 1.0);
  vec4 x12 = x0.xyxy + C.xxzz;
  x12.xy -= i1;
// Permutations
  i = mod289(i); // Avoid truncation effects in permutation
  vec3 p = permute( permute( i.y + vec3(0.0, i1.y, 1.0 ))
		+ i.x + vec3(0.0, i1.x, 1.0 ));
  vec3 m = max(0.5 - vec3(dot(x0,x0), dot(x12.xy,x12.xy), dot(x12.zw,x12.zw)), 0.0);
  m = m*m ;
  m = m*m ;
// Gradients: 41 points uniformly over a line, mapped onto a diamond.
// The ring size 17*17 = 289 is close to a multiple of 41 (41*7 = 287)
  vec3 x = 2.0 * fract(p * C.www) - 1.0;
  vec3 h = abs(x) - 0.5;
  vec3 ox = floor(x + 0.5);
  vec3 a0 = x - ox;
// Normalise gradients implicitly by scaling m
// Approximation of: m *= inversesqrt( a0*a0 + h*h );
  m *= 1.79284291400159 - 0.85373472095314 * ( a0*a0 + h*h );
// Compute final noise value at P
  vec3 g;
  g.x  = a0.x  * x0.x  + h.x  * x0.y;
  g.yz = a0.yz * x12.xz + h.yz * x12.yw;
  return 130.0 * dot(m, g);
}

float staticV(vec2 uv) {
    float staticHeight = snoise(vec2(9.0,iTime*1.2+3.0))*0.3+5.0;
    float staticAmount = snoise(vec2(1.0,iTime*1.2-6.0))*0.1+0.3;
    float staticStrength = snoise(vec2(-9.75,iTime*0.6-3.0))*2.0+2.0;
	return (1.0-step(snoise(vec2(5.0*pow(iTime,2.0)+pow(uv.x*7.0,1.2),pow((mod(iTime,100.0)+100.0)*uv.y*0.3+3.0,staticHeight))),staticAmount))*staticStrength;
}

// Barrel distortion function
vec2 barrelDistortion(vec2 coord, float power) {
    vec2 cc = coord - 0.5;
    float dist = dot(cc, cc);
    return coord + cc * (1.0 + power * dist) * dist * power;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    // Get texture coordinates
    vec2 uv = fragCoord.xy / iResolution.xy;

    float staticVal = 0.0;
    for (float y = -1.0; y <= 1.0; y += 1.0) {
        float maxDist = 5.0/200.0;
        float dist = y/200.0;
        staticVal += staticV(vec2(uv.x,uv.y+dist))*(maxDist-abs(dist))*1.5;
    }

#ifdef CURVE
    // Apply barrel distortion
    uv = barrelDistortion(uv, CURVE);
#endif

    // Apply Distorted TV effects
    float jerkOffset = (1.0-step(snoise(vec2(iTime*1.3,5.0)),0.8))*0.05;
    float fuzzOffset = snoise(vec2(iTime*15.0,uv.y*80.0))*0.003;
    float largeFuzzOffset = snoise(vec2(iTime*1.0,uv.y*25.0))*0.004;
    
    float vertMovementOn = (1.0-step(snoise(vec2(iTime*0.2,8.0)),0.4))*vertMovementOpt;
    float vertJerk = (1.0-step(snoise(vec2(iTime*1.5,5.0)),0.6))*vertJerkOpt;
    float vertJerk2 = (1.0-step(snoise(vec2(iTime*5.5,5.0)),0.2))*vertJerkOpt;
    float yOffset = abs(sin(iTime)*4.0)*vertMovementOn+vertJerk*vertJerk2*0.3;
    float y = mod(uv.y+yOffset,1.0);
    
    float xOffset = (fuzzOffset + largeFuzzOffset) * horzFuzzOpt;
    
    staticVal *= bottomStaticOpt;
    
    // RGB color fringing from Distorted TV
    fragColor.r = texture(iChannel0, vec2(uv.x + xOffset -0.01*rgbOffsetOpt, y)).r + staticVal;
    fragColor.g = texture(iChannel0, vec2(uv.x + xOffset, y)).g + staticVal;
    fragColor.b = texture(iChannel0, vec2(uv.x + xOffset +0.01*rgbOffsetOpt, y)).b + staticVal;
    fragColor.a = texture(iChannel0, uv).a;

    // Quadratically darken everything
    fragColor.rgb = mix(fragColor.rgb, fragColor.rgb*fragColor.rgb, DARKEN_MIX);

    // Vignette effect
    fragColor.rgb *= VIGNETTE_BRIGHTNESS * pow(uv.x * uv.y * (1.0-uv.x) * (1.0-uv.y), VIGNETTE_SPREAD);

    // Tint all colors
    fragColor.rgb *= vec3(TINT);

    // Apply scanlines from Distorted TV
    float scanline = sin(uv.y*800.0)*0.04*scalinesOpt;
    fragColor.rgb -= scanline;

    // Add flicker
    fragColor *= 1.0 - FLICKER_STRENGTH/2.0*(1.0 + sin(2*PI*FLICKER_FREQUENCY*iTime));

    // Remove output outside of screen bounds
    if (uv.x < 0.0 || uv.x > 1.0)
        fragColor.rgb *= 0.0;
    if (uv.y < 0.0 || uv.y > 1.0)
        fragColor.rgb *= 0.0;

#ifdef BLOOM_SPREAD
    // Add bloom
    vec2 step = BLOOM_SPREAD * vec2(1.414) / iResolution.xy;

    for (int i = 0; i < 24; i++) {
        vec3 bloom_sample = bloom_samples[i];
        vec4 neighbor = texture(iChannel0, uv + bloom_sample.xy * step);
        float luminance = 0.299 * neighbor.r + 0.587 * neighbor.g + 0.114 * neighbor.b;

        fragColor += luminance * bloom_sample.z * neighbor * BLOOM_STRENGTH;
    }

    fragColor = clamp(fragColor, 0.0, 1.0);
#endif

    // Add fade effect to smoothen out color transitions
    fragColor = vec4(FADE_FACTOR*fragColor.rgb, FADE_FACTOR);
}