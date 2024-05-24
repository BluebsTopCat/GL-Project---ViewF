// Made with Amplify Shader Editor
// Available at the Unity Asset Store - http://u3d.as/y3X 
Shader "Shad_Sky"
{
	Properties
	{
		
	}
	
	SubShader
	{
		
		
		Tags { "RenderType"="Opaque" }
	LOD 100

		CGINCLUDE
		#pragma target 3.0
		ENDCG
		Blend Off
		AlphaToMask Off
		Cull Back
		ColorMask RGBA
		ZWrite On
		ZTest LEqual
		Offset 0 , 0
		
		
		
		Pass
		{
			Name "Unlit"
			Tags { "LightMode"="ForwardBase" }
			CGPROGRAM

			

			#ifndef UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX
			//only defining to not throw compilation error over Unity 5.5
			#define UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(input)
			#endif
			#pragma vertex vert
			#pragma fragment frag
			#pragma multi_compile_instancing
			#include "UnityCG.cginc"
			#include "Lighting.cginc"
			#include "AutoLight.cginc"
			#include "UnityStandardBRDF.cginc"
			#include "UnityShaderVariables.cginc"
			#define ASE_NEEDS_FRAG_WORLD_POSITION


			struct appdata
			{
				float4 vertex : POSITION;
				float4 color : COLOR;
				float4 ase_tangent : TANGENT;
				float3 ase_normal : NORMAL;
				UNITY_VERTEX_INPUT_INSTANCE_ID
			};
			
			struct v2f
			{
				float4 vertex : SV_POSITION;
				#ifdef ASE_NEEDS_FRAG_WORLD_POSITION
				float3 worldPos : TEXCOORD0;
				#endif
				float4 ase_texcoord1 : TEXCOORD1;
				float4 ase_texcoord2 : TEXCOORD2;
				float4 ase_texcoord3 : TEXCOORD3;
				UNITY_VERTEX_INPUT_INSTANCE_ID
				UNITY_VERTEX_OUTPUT_STEREO
			};

			//This is a late directive
			
			uniform float4 Sky_Top;
			uniform float4 Sky_Bot;
			uniform float Time;
			struct Gradient
			{
				int type;
				int colorsLength;
				int alphasLength;
				float4 colors[8];
				float2 alphas[8];
			};
			
			Gradient NewGradient(int type, int colorsLength, int alphasLength, 
			float4 colors0, float4 colors1, float4 colors2, float4 colors3, float4 colors4, float4 colors5, float4 colors6, float4 colors7,
			float2 alphas0, float2 alphas1, float2 alphas2, float2 alphas3, float2 alphas4, float2 alphas5, float2 alphas6, float2 alphas7)
			{
				Gradient g;
				g.type = type;
				g.colorsLength = colorsLength;
				g.alphasLength = alphasLength;
				g.colors[ 0 ] = colors0;
				g.colors[ 1 ] = colors1;
				g.colors[ 2 ] = colors2;
				g.colors[ 3 ] = colors3;
				g.colors[ 4 ] = colors4;
				g.colors[ 5 ] = colors5;
				g.colors[ 6 ] = colors6;
				g.colors[ 7 ] = colors7;
				g.alphas[ 0 ] = alphas0;
				g.alphas[ 1 ] = alphas1;
				g.alphas[ 2 ] = alphas2;
				g.alphas[ 3 ] = alphas3;
				g.alphas[ 4 ] = alphas4;
				g.alphas[ 5 ] = alphas5;
				g.alphas[ 6 ] = alphas6;
				g.alphas[ 7 ] = alphas7;
				return g;
			}
			
			float4 SampleGradient( Gradient gradient, float time )
			{
				float3 color = gradient.colors[0].rgb;
				UNITY_UNROLL
				for (int c = 1; c < 8; c++)
				{
				float colorPos = saturate((time - gradient.colors[c-1].w) / ( 0.00001 + (gradient.colors[c].w - gradient.colors[c-1].w)) * step(c, (float)gradient.colorsLength-1));
				color = lerp(color, gradient.colors[c].rgb, lerp(colorPos, step(0.01, colorPos), gradient.type));
				}
				#ifndef UNITY_COLORSPACE_GAMMA
				color = half3(GammaToLinearSpaceExact(color.r), GammaToLinearSpaceExact(color.g), GammaToLinearSpaceExact(color.b));
				#endif
				float alpha = gradient.alphas[0].x;
				UNITY_UNROLL
				for (int a = 1; a < 8; a++)
				{
				float alphaPos = saturate((time - gradient.alphas[a-1].y) / ( 0.00001 + (gradient.alphas[a].y - gradient.alphas[a-1].y)) * step(a, (float)gradient.alphasLength-1));
				alpha = lerp(alpha, gradient.alphas[a].x, lerp(alphaPos, step(0.01, alphaPos), gradient.type));
				}
				return float4(color, alpha);
			}
			
					float2 voronoihash93( float2 p )
					{
						
						p = float2( dot( p, float2( 127.1, 311.7 ) ), dot( p, float2( 269.5, 183.3 ) ) );
						return frac( sin( p ) *43758.5453);
					}
			
					float voronoi93( float2 v, float time, inout float2 id, inout float2 mr, float smoothness, inout float2 smoothId )
					{
						float2 n = floor( v );
						float2 f = frac( v );
						float F1 = 8.0;
						float F2 = 8.0; float2 mg = 0;
						for ( int j = -1; j <= 1; j++ )
						{
							for ( int i = -1; i <= 1; i++ )
						 	{
						 		float2 g = float2( i, j );
						 		float2 o = voronoihash93( n + g );
								o = ( sin( time + o * 6.2831 ) * 0.5 + 0.5 ); float2 r = f - g - o;
								float d = 0.5 * dot( r, r );
						 		if( d<F1 ) {
						 			F2 = F1;
						 			F1 = d; mg = g; mr = r; id = o;
						 		} else if( d<F2 ) {
						 			F2 = d;
						
						 		}
						 	}
						}
						return F1;
					}
			

			
			v2f vert ( appdata v )
			{
				v2f o;
				UNITY_SETUP_INSTANCE_ID(v);
				UNITY_INITIALIZE_VERTEX_OUTPUT_STEREO(o);
				UNITY_TRANSFER_INSTANCE_ID(v, o);

				float3 ase_worldTangent = UnityObjectToWorldDir(v.ase_tangent);
				o.ase_texcoord1.xyz = ase_worldTangent;
				float3 ase_worldNormal = UnityObjectToWorldNormal(v.ase_normal);
				o.ase_texcoord2.xyz = ase_worldNormal;
				float ase_vertexTangentSign = v.ase_tangent.w * unity_WorldTransformParams.w;
				float3 ase_worldBitangent = cross( ase_worldNormal, ase_worldTangent ) * ase_vertexTangentSign;
				o.ase_texcoord3.xyz = ase_worldBitangent;
				
				
				//setting value to unused interpolator channels and avoid initialization warnings
				o.ase_texcoord1.w = 0;
				o.ase_texcoord2.w = 0;
				o.ase_texcoord3.w = 0;
				float3 vertexValue = float3(0, 0, 0);
				#if ASE_ABSOLUTE_VERTEX_POS
				vertexValue = v.vertex.xyz;
				#endif
				vertexValue = vertexValue;
				#if ASE_ABSOLUTE_VERTEX_POS
				v.vertex.xyz = vertexValue;
				#else
				v.vertex.xyz += vertexValue;
				#endif
				o.vertex = UnityObjectToClipPos(v.vertex);

				#ifdef ASE_NEEDS_FRAG_WORLD_POSITION
				o.worldPos = mul(unity_ObjectToWorld, v.vertex).xyz;
				#endif
				return o;
			}
			
			fixed4 frag (v2f i ) : SV_Target
			{
				UNITY_SETUP_INSTANCE_ID(i);
				UNITY_SETUP_STEREO_EYE_INDEX_POST_VERTEX(i);
				fixed4 finalColor;
				#ifdef ASE_NEEDS_FRAG_WORLD_POSITION
				float3 WorldPosition = i.worldPos;
				#endif
				Gradient gradient78 = NewGradient( 0, 6, 2, float4( 0, 0, 0, 0 ), float4( 0.04411765, 0.04411765, 0.04411765, 0.6852979 ), float4( 0.04916547, 0.04916547, 0.04916547, 0.7823606 ), float4( 0.1442145, 0.1442145, 0.1442145, 0.9000076 ), float4( 0.3275073, 0.3275073, 0.3275073, 0.9794156 ), float4( 1, 1, 1, 1 ), 0, 0, float2( 1, 0 ), float2( 1, 1 ), 0, 0, 0, 0, 0, 0 );
				float3 worldSpaceLightDir = UnityWorldSpaceLightDir(WorldPosition);
				float3 ase_worldViewDir = UnityWorldSpaceViewDir(WorldPosition);
				ase_worldViewDir = normalize(ase_worldViewDir);
				float dotResult27 = dot( -worldSpaceLightDir , ase_worldViewDir );
				float temp_output_31_0 = saturate( dotResult27 );
				float dotResult32 = dot( temp_output_31_0 , temp_output_31_0 );
				float4 SunBloom275 = SampleGradient( gradient78, dotResult32 );
				float temp_output_62_0 = ( 1.0 - step( dotResult32 , 0.998 ) );
				float4 temp_cast_0 = (temp_output_62_0).xxxx;
				float4 SkyTop51 = Sky_Top;
				float4 lerpResult57 = lerp( temp_cast_0 , ( temp_output_62_0 * SkyTop51 ) , 0.1);
				float4 SunDisk59 = ( lerpResult57 * 5.0 );
				float4 SkyBot52 = Sky_Bot;
				ase_worldViewDir = Unity_SafeNormalize( ase_worldViewDir );
				float clampResult49 = clamp( ( dotResult32 - 0.75 ) , 0.0 , 2.0 );
				float Sunbloom47 = clampResult49;
				float clampResult22 = clamp( ( ( ase_worldViewDir.y + 1.0 ) - Sunbloom47 ) , 0.0 , 1.0 );
				float4 lerpResult19 = lerp( SkyTop51 , SkyBot52 , clampResult22);
				float4 lerpResult64 = lerp( SunDisk59 , lerpResult19 , step( SunDisk59.r , 0.0 ));
				Gradient gradient113 = NewGradient( 0, 4, 2, float4( 1, 1, 1, 0 ), float4( 0.03260869, 0.03260869, 0.03260869, 0.2970626 ), float4( 0, 0, 0, 0.7000076 ), float4( 1, 1, 1, 1 ), 0, 0, 0, 0, float2( 1, 0 ), float2( 1, 1 ), 0, 0, 0, 0, 0, 0 );
				float time93 = 0.0;
				float2 voronoiSmoothId93 = 0;
				float3 ase_worldTangent = i.ase_texcoord1.xyz;
				float3 ase_worldNormal = i.ase_texcoord2.xyz;
				float3 ase_worldBitangent = i.ase_texcoord3.xyz;
				float3 tanToWorld0 = float3( ase_worldTangent.x, ase_worldBitangent.x, ase_worldNormal.x );
				float3 tanToWorld1 = float3( ase_worldTangent.y, ase_worldBitangent.y, ase_worldNormal.y );
				float3 tanToWorld2 = float3( ase_worldTangent.z, ase_worldBitangent.z, ase_worldNormal.z );
				float3 ase_tanViewDir =  tanToWorld0 * ase_worldViewDir.x + tanToWorld1 * ase_worldViewDir.y  + tanToWorld2 * ase_worldViewDir.z;
				ase_tanViewDir = normalize(ase_tanViewDir);
				float mulTime109 = _Time.y * 0.0001;
				float2 appendResult94 = (float2(ase_tanViewDir.z , ( mulTime109 + ase_tanViewDir.x )));
				float2 coords93 = appendResult94 * 50.0;
				float2 id93 = 0;
				float2 uv93 = 0;
				float voroi93 = voronoi93( coords93, time93, id93, uv93, 0, voronoiSmoothId93 );
				Gradient gradient97 = NewGradient( 0, 6, 2, float4( 0.05660379, 0.05660379, 0.05660379, 0 ), float4( 1, 1, 1, 0.3911803 ), float4( 1, 0.0990566, 0.0990566, 0.4705882 ), float4( 0.1441794, 0.1913593, 0.6792453, 0.5764706 ), float4( 1, 1, 1, 0.6705883 ), float4( 0, 0, 0, 1 ), 0, 0, float2( 1, 0 ), float2( 1, 1 ), 0, 0, 0, 0, 0, 0 );
				float4 Stars111 = ( step( ( 1.0 - voroi93 ) , 0.5 ) * SampleGradient( gradient97, uv93.x ) );
				
				
				finalColor = ( SunBloom275 + lerpResult64 + ( SampleGradient( gradient113, ( Time / 24.0 ) ) * Stars111 ) );
				return finalColor;
			}
			ENDCG
		}
	}
	CustomEditor "ASEMaterialInspector"
	
	
}
/*ASEBEGIN
Version=18935
0;962;1348;29;1341.731;447.8222;1.951924;True;False
Node;AmplifyShaderEditor.WorldSpaceLightDirHlpNode;25;-1489.264,742.5957;Inherit;False;False;1;0;FLOAT;0;False;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.ViewDirInputsCoordNode;28;-1488.83,886.4717;Float;False;World;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.NegateNode;26;-1226.198,873.5767;Inherit;False;1;0;FLOAT3;0,0,0;False;1;FLOAT3;0
Node;AmplifyShaderEditor.DotProductOpNode;27;-1053.675,874.7927;Inherit;False;2;0;FLOAT3;0,0,0;False;1;FLOAT3;0,0,0;False;1;FLOAT;0
Node;AmplifyShaderEditor.SaturateNode;31;-882.4955,810.476;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.DotProductOpNode;32;-672.5251,746.5549;Inherit;True;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;17;-945.2085,-488.0169;Inherit;False;Global;Sky_Top;Sky_Top;0;0;Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0.01176471,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.StepOpNode;50;-371.8387,827.0812;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.998;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleTimeNode;109;-604.0556,-673.6961;Inherit;False;1;0;FLOAT;0.0001;False;1;FLOAT;0
Node;AmplifyShaderEditor.ViewDirInputsCoordNode;88;-559.2242,-499.6992;Inherit;False;Tangent;False;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.RegisterLocalVarNode;51;-757.1592,-581.6852;Inherit;False;SkyTop;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;110;-431.0556,-598.6961;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;62;-194.3423,848.3392;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;56;-300.8266,985.8087;Inherit;False;51;SkyTop;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;44;-254.0698,706.549;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.75;False;1;FLOAT;0
Node;AmplifyShaderEditor.RangedFloatNode;58;117.7251,1020.472;Inherit;False;Constant;_Float0;Float 0;0;0;Create;True;0;0;0;False;0;False;0.1;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;55;-30.62519,958.634;Inherit;False;2;2;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.DynamicAppendNode;94;-210.6242,-517.5992;Inherit;False;FLOAT2;4;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;0;False;3;FLOAT;0;False;1;FLOAT2;0
Node;AmplifyShaderEditor.RangedFloatNode;71;391.2326,956.6367;Inherit;False;Constant;_Float1;Float 1;0;0;Create;True;0;0;0;False;0;False;5;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.LerpOp;57;147.7253,845.8328;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.ClampOpNode;49;111.8442,635.15;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;2;False;1;FLOAT;0
Node;AmplifyShaderEditor.VoronoiNode;93;-31.22411,-607.2993;Inherit;False;0;0;1;0;1;False;1;False;False;False;4;0;FLOAT2;0,0;False;1;FLOAT;0;False;2;FLOAT;50;False;3;FLOAT;0;False;3;FLOAT;0;FLOAT2;1;FLOAT2;2
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;70;429.7333,835.3596;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;47;382.9147,665.2482;Inherit;False;Sunbloom;-1;True;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.OneMinusNode;95;251.6756,-649.1992;Inherit;False;1;0;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GradientNode;97;179.3758,-543.5992;Inherit;False;0;6;2;0.05660379,0.05660379,0.05660379,0;1,1,1,0.3911803;1,0.0990566,0.0990566,0.4705882;0.1441794,0.1913593,0.6792453,0.5764706;1,1,1,0.6705883;0,0,0,1;1,0;1,1;0;1;OBJECT;0
Node;AmplifyShaderEditor.ViewDirInputsCoordNode;12;-848.3954,228.6691;Inherit;False;World;True;0;4;FLOAT3;0;FLOAT;1;FLOAT;2;FLOAT;3
Node;AmplifyShaderEditor.GradientSampleNode;98;480.6754,-544.6992;Inherit;True;2;0;OBJECT;;False;1;FLOAT;0;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.RegisterLocalVarNode;59;668.6317,826.6334;Inherit;False;SunDisk;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;16;-583.2971,200.392;Inherit;False;2;2;0;FLOAT;0;False;1;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;48;-570.0803,315.6643;Inherit;False;47;Sunbloom;1;0;OBJECT;;False;1;FLOAT;0
Node;AmplifyShaderEditor.StepOpNode;96;413.3755,-682.606;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0.5;False;1;FLOAT;0
Node;AmplifyShaderEditor.ColorNode;21;-926.5057,-323.6331;Inherit;False;Global;Sky_Bot;Sky_Bot;0;0;Create;True;0;0;0;False;0;False;0,0,0,0;0,0,0,1;True;0;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.GradientNode;78;-694.7087,619.9297;Inherit;False;0;6;2;0,0,0,0;0.04411765,0.04411765,0.04411765,0.6852979;0.04916547,0.04916547,0.04916547,0.7823606;0.1442145,0.1442145,0.1442145,0.9000076;0.3275073,0.3275073,0.3275073,0.9794156;1,1,1,1;1,0;1,1;0;1;OBJECT;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;99;746.4715,-670.9984;Inherit;False;2;2;0;FLOAT;0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleSubtractOpNode;46;-376.4248,211.797;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;52;-628.0544,-355.0424;Inherit;False;SkyBot;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.RangedFloatNode;115;-156.7059,-250.7424;Inherit;False;Global;Time;Time;0;0;Create;True;0;0;0;False;0;False;0;0;0;0;0;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;61;39.40271,16.53737;Inherit;False;59;SunDisk;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GradientSampleNode;79;-469.5088,529.2296;Inherit;True;2;0;OBJECT;;False;1;FLOAT;0;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.BreakToComponentsNode;66;289.4251,271.782;Inherit;False;COLOR;1;0;COLOR;0,0,0,0;False;16;FLOAT;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4;FLOAT;5;FLOAT;6;FLOAT;7;FLOAT;8;FLOAT;9;FLOAT;10;FLOAT;11;FLOAT;12;FLOAT;13;FLOAT;14;FLOAT;15
Node;AmplifyShaderEditor.RegisterLocalVarNode;111;991.9597,-673.3032;Inherit;False;Stars;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.ClampOpNode;22;-223.4611,166.6839;Inherit;False;3;0;FLOAT;0;False;1;FLOAT;0;False;2;FLOAT;1;False;1;FLOAT;0
Node;AmplifyShaderEditor.SimpleDivideOpNode;116;57.80689,-253.7298;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;24;False;1;FLOAT;0
Node;AmplifyShaderEditor.GradientNode;113;-164.5867,-348.4652;Inherit;False;0;4;2;1,1,1,0;0.03260869,0.03260869,0.03260869,0.2970626;0,0,0,0.7000076;1,1,1,1;1,0;1,1;0;1;OBJECT;0
Node;AmplifyShaderEditor.GetLocalVarNode;53;-205.059,-4.751306;Inherit;False;51;SkyTop;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;54;-216.059,62.2487;Inherit;False;52;SkyBot;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.GradientSampleNode;114;164.8336,-351.6176;Inherit;True;2;0;OBJECT;;False;1;FLOAT;0;False;5;COLOR;0;FLOAT;1;FLOAT;2;FLOAT;3;FLOAT;4
Node;AmplifyShaderEditor.LerpOp;19;39.39508,112.8798;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.StepOpNode;65;445.7489,226.5643;Inherit;False;2;0;FLOAT;0;False;1;FLOAT;0;False;1;FLOAT;0
Node;AmplifyShaderEditor.GetLocalVarNode;112;300.6048,-141.2814;Inherit;False;111;Stars;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.RegisterLocalVarNode;75;-101.8594,526.8096;Inherit;False;SunBloom2;-1;True;1;0;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.LerpOp;64;618.9492,31.82471;Inherit;False;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;FLOAT;0;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleMultiplyOpNode;117;684.1524,-302.286;Inherit;False;2;2;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.GetLocalVarNode;73;507.6455,-97.96424;Inherit;False;75;SunBloom2;1;0;OBJECT;;False;1;COLOR;0
Node;AmplifyShaderEditor.SimpleAddOpNode;72;931.8488,-82.54108;Inherit;False;3;3;0;COLOR;0,0,0,0;False;1;COLOR;0,0,0,0;False;2;COLOR;0,0,0,0;False;1;COLOR;0
Node;AmplifyShaderEditor.TemplateMultiPassMasterNode;6;1321.56,-80.63058;Float;False;True;-1;2;ASEMaterialInspector;100;1;Shad_Sky;0770190933193b94aaa3065e307002fa;True;Unlit;0;0;Unlit;2;False;True;0;1;False;-1;0;False;-1;0;1;False;-1;0;False;-1;True;0;False;-1;0;False;-1;False;False;False;False;False;False;False;False;False;True;0;False;-1;False;True;0;False;-1;False;True;True;True;True;True;0;False;-1;False;False;False;False;False;False;False;True;False;255;False;-1;255;False;-1;255;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;7;False;-1;1;False;-1;1;False;-1;1;False;-1;False;True;1;False;-1;True;3;False;-1;True;True;0;False;-1;0;False;-1;True;1;RenderType=Opaque=RenderType;True;2;False;0;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;False;True;1;LightMode=ForwardBase;False;False;0;;0;0;Standard;1;Vertex Position,InvertActionOnDeselection;1;0;0;1;True;False;;False;0
WireConnection;26;0;25;0
WireConnection;27;0;26;0
WireConnection;27;1;28;0
WireConnection;31;0;27;0
WireConnection;32;0;31;0
WireConnection;32;1;31;0
WireConnection;50;0;32;0
WireConnection;51;0;17;0
WireConnection;110;0;109;0
WireConnection;110;1;88;1
WireConnection;62;0;50;0
WireConnection;44;0;32;0
WireConnection;55;0;62;0
WireConnection;55;1;56;0
WireConnection;94;0;88;3
WireConnection;94;1;110;0
WireConnection;57;0;62;0
WireConnection;57;1;55;0
WireConnection;57;2;58;0
WireConnection;49;0;44;0
WireConnection;93;0;94;0
WireConnection;70;0;57;0
WireConnection;70;1;71;0
WireConnection;47;0;49;0
WireConnection;95;0;93;0
WireConnection;98;0;97;0
WireConnection;98;1;93;2
WireConnection;59;0;70;0
WireConnection;16;0;12;2
WireConnection;96;0;95;0
WireConnection;99;0;96;0
WireConnection;99;1;98;0
WireConnection;46;0;16;0
WireConnection;46;1;48;0
WireConnection;52;0;21;0
WireConnection;79;0;78;0
WireConnection;79;1;32;0
WireConnection;66;0;61;0
WireConnection;111;0;99;0
WireConnection;22;0;46;0
WireConnection;116;0;115;0
WireConnection;114;0;113;0
WireConnection;114;1;116;0
WireConnection;19;0;53;0
WireConnection;19;1;54;0
WireConnection;19;2;22;0
WireConnection;65;0;66;0
WireConnection;75;0;79;0
WireConnection;64;0;61;0
WireConnection;64;1;19;0
WireConnection;64;2;65;0
WireConnection;117;0;114;0
WireConnection;117;1;112;0
WireConnection;72;0;73;0
WireConnection;72;1;64;0
WireConnection;72;2;117;0
WireConnection;6;0;72;0
ASEEND*/
//CHKSM=A10D2476401951B67F73D0274411EF34B3BCAE65