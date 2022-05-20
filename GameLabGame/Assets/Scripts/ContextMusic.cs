using System;
using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;

public class ContextMusic : MonoBehaviour
{
    public List<musicbit> clips;
    private GameObject player;
    public float inmutezone = 0; 

    public bool show;

    private List<musicbit> muted;
    private List<musicbit> unmuted;
    // Start is called befo  re the first frame update
    void Start()
    {
        player = GameObject.Find("Player");
        foreach (musicbit bit in clips)
        {
            GameObject g = new GameObject();
            g.transform.parent = this.transform;
            g.transform.position = bit.center;
            bit.audio = g.AddComponent<AudioSource>();
            if (!bit.mute)
            {
                bit.audio.loop = true;
                bit.audio.clip = bit.song;
                bit.audio.Play();
            }

            if (bit.square)
            {
                BoxCollider c = g.AddComponent<BoxCollider>();
                c.size = bit.squaresize;
                c.isTrigger = true;
                bit.c = c;
            }
        }
        muted = clips.FindAll(o => o.mute);
        unmuted = clips.FindAll(o => !o.mute);
    }

    private void Update()
    {
        float subt = 0;
        if (muted.Count != 0)
        {
            foreach (musicbit b in muted)
            {
                if (b.square)
                {
                    if (b.c.bounds.Contains(player.transform.position))
                    {
                        subt += b.falloff.Evaluate(0);
                    }
                    else
                    {
                        subt += b.falloff.Evaluate(Vector3.Distance(b.c.ClosestPointOnBounds(player.transform.position),
                            player.transform.position));
                    }
                }
                else
                {
                    subt += b.falloff.Evaluate(Vector3.Distance(b.audio.gameObject.transform.position,
                        player.transform.position));
                }

                inmutezone = 1 - Mathf.Clamp(subt, 0, 1);

            }
        }
        else
        {
            inmutezone = 1;
        }

        foreach (musicbit b in unmuted)
        {
            if (b.square)
            {
                if (b.c.bounds.Contains(player.transform.position))
                {
                    Debug.Log("Playerinthere");
                    b.audio.volume = b.falloff.Evaluate(0) * inmutezone;
                }
                else
                {
                    b.audio.volume = b.falloff.Evaluate(Vector3.Distance(b.c.ClosestPointOnBounds(player.transform.position), player.transform.position)) * inmutezone;
                }
            }
            else
            {
                b.audio.volume = b.falloff.Evaluate(Vector3.Distance(b.audio.gameObject.transform.position, player.transform.position)) * inmutezone;
            }
            
        }
    }

    private void OnDrawGizmos()
    {
        foreach (musicbit b in clips)
        {
            Gizmos.color = b.color;
            if (b.show)
            {
                if(b.square)
                    Gizmos.DrawWireCube(b.center,b.squaresize);
                else
                    Gizmos.DrawWireSphere(b.center, b.falloff.keys[b.falloff.keys.Length -1].time);

                if (show)
                {
                    if (b.square)
                        Gizmos.DrawCube(b.center, b.squaresize);
                    else
                        Gizmos.DrawSphere(b.center, b.falloff.keys[b.falloff.keys.Length -1].time);
                }

               
            }
            
            Gizmos.DrawSphere(b.center,1);
        }
    }
}
[System.Serializable]
public class musicbit
{
    public String name;
    public Vector3 center;
    public AnimationCurve falloff;
    public AudioClip song;
    [HideInInspector]
    public AudioSource audio;
    public bool mute;
    public bool show;
    public Color color;
    public bool square;
    public Vector3 squaresize;
    public Collider c;

    public musicbit(string str,Vector3 c, AnimationCurve f, AudioClip s, Color co, bool b, bool m, bool sq, Vector3 sqsz)
    {
        name = str;
        center = c;
        falloff = f;
        song = s;
        color = co;
        show = b;
        mute = m;
        square = sq;
        squaresize = sqsz;
    }

}