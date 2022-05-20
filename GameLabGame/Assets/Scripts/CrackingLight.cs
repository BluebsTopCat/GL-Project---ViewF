using System.Collections;
using System.Collections.Generic;
using UnityEngine;

[RequireComponent(typeof(Light))]
[ExecuteAlways]
public class CrackingLight : MonoBehaviour
{
    public float strength = 5;
    public float variability = 3;
    public float range = 10;
    public float rangevariation = 5;
    public Gradient color;
    public float speed;

    private Light l;
    // Start is called before the first frame update
    void Start()
    {
        l = this.GetComponent<Light>();
    }

    // Update is called once per frame
    void Update()
    {
        float adjustedtime = Time.time * speed;
        float currentstr = (2 * Mathf.Sin(adjustedtime) + Mathf.Sin(adjustedtime * 5) +
                            Mathf.Cos(13 * adjustedtime) / 2 + 2 * Mathf.Sin(adjustedtime / 10)) / 5;
        l.color = color.Evaluate(Mathf.Clamp(currentstr,0,1));
        l.intensity = strength + 2 * variability * (currentstr - .5f);
        l.range = range + 2 * rangevariation * (currentstr - .5f);
    }
}
