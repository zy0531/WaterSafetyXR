Shader "Custom/SeeThroughWater"
{
    Properties
    {
        _RefractionStrength("Refraction Strength", Range(0, 0.5)) = 0.2
        _NormalMap("Normal Map", 2D) = "bump" {}
        _DistortionScale("Distortion Scale", Range(0.01, 2.0)) = 0.5
    }
        SubShader
        {
            Tags { "RenderType" = "Transparent" "Queue" = "Transparent" }
            LOD 200

            Pass
            {
                Name "DistortionPass"
                Blend SrcAlpha OneMinusSrcAlpha
                ZWrite Off
                Cull Off

                CGPROGRAM
                #pragma vertex vert
                #pragma fragment frag
                #include "UnityCG.cginc"

                sampler2D _CameraOpaqueTexture;
                sampler2D _NormalMap;
                float _RefractionStrength;
                float _DistortionScale;

                struct appdata
                {
                    float4 vertex : POSITION;
                    float2 uv : TEXCOORD0;
                };

                struct v2f
                {
                    float4 pos : SV_POSITION;
                    float2 uv : TEXCOORD0;
                    float2 uvDistort : TEXCOORD1;
                };

                v2f vert(appdata v)
                {
                    v2f o;
                    o.pos = UnityObjectToClipPos(v.vertex);
                    o.uv = v.uv;
                    o.uvDistort = v.uv * _DistortionScale;
                    return o;
                }

                half4 frag(v2f i) : SV_Target
                {
                    // Sample the normal map for distortion
                    float3 normal = UnpackNormal(tex2D(_NormalMap, i.uvDistort));
                    float2 distortion = normal.xy * (_RefractionStrength * 0.1);

                    // Offset UVs by the distortion for refraction effect
                    float2 uvDistorted = i.uv + distortion;
                    half4 color = tex2D(_CameraOpaqueTexture, uvDistorted);

                    // Set plane itself as transparent
                    color.a = 0;
                    return color;
                }
                ENDCG
            }
        }
            FallBack "Transparent"
}
