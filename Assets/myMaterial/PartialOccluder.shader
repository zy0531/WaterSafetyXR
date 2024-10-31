Shader "Custom/PartialOccluder"
{
    SubShader
    {
        PackageRequirements
        {
            "com.unity.render-pipelines.universal"
        }
        Tags { "RenderPipeline" = "UniversalPipeline"  "Queue" = "Transparent" }

        Pass
        {
            Name "ForwardLit"
            Tags { "LightMode" = "UniversalForward" }

            Cull Off
            ZWrite On
            ZTest Less
            Blend SrcAlpha OneMinusSrcAlpha // Enable partial transparency

            HLSLPROGRAM
            #pragma prefer_hlslcc gles
            #pragma exclude_renderers d3d11_9x
            #pragma target 2.0
            #pragma multi_compile_instancing

            #pragma vertex LitPassVertex
            #pragma fragment LitPassFragment

            #include "Packages/com.unity.render-pipelines.universal/Shaders/LitInput.hlsl"
            #include "Packages/com.unity.render-pipelines.universal/Shaders/LitForwardPass.hlsl"

        // Define a transparency level for partial occlusion
        float _Transparency = 0.5; // Adjust to control occlusion strength (0.0 to 1.0)

        // Define the v2f structure within this pass
        struct v2f {
            float4 vertex : SV_POSITION;
        };

        float4 LitPassFragment(v2f i) : SV_Target {
            return float4(0, 0, 0, _Transparency); // Set color with partial transparency
        }
        ENDHLSL
    }
    }

        SubShader
        {
            Tags { "RenderType" = "Transparent" }
            LOD 100
            Cull Off
            ZWrite On
            ZTest Less
            Blend SrcAlpha OneMinusSrcAlpha // Enable partial transparency

            Pass
            {
                CGPROGRAM
                #pragma vertex vert
                #pragma fragment frag
                #pragma multi_compile_fog
                #include "UnityCG.cginc"

                // Define the v2f structure within this pass
                struct appdata {
                    float4 vertex : POSITION;
                };

                struct v2f {
                    float4 vertex : SV_POSITION;
                };

                v2f vert(appdata v) {
                    v2f o;
                    o.vertex = UnityObjectToClipPos(v.vertex);
                    return o;
                }

                // Set the fragment shader to return a partially transparent color
                float4 frag(v2f i) : SV_Target {
                    return float4(0, 0, 0, 0.5); // Adjust alpha for occlusion strength
                }
                ENDCG
            }
        }
}
