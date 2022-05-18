using System.Collections;
using System.Collections.Generic;
using UnityEngine;


[ExecuteAlways]
public class GrassShader : MonoBehaviour
{
    private static readonly int Position = Shader.PropertyToID("Position");
    
    void Update()
    {
        Shader.SetGlobalVector(Position, this.transform.position);
    }
}
