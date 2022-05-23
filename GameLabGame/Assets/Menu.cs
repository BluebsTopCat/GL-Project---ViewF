using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UI;

public class Menu : MonoBehaviour
{
    public Animator Menuanimation;

    public enum oobtype
    {
        Water,Fall
    }
    public Player p;
    public Image oobima;
    public Sprite fall;
    public Sprite water;

    public AudioSource ads;
    public AudioClip fallclip;
    public AudioClip waterclip;
    // Start is called before the first frame update
    void Start()
    {
        p = GameObject.FindObjectOfType<Player>();
    }

    // Update is called once per frame
    public void outofbounds(oobtype ob)
    {
        switch (ob)
        {
            case oobtype.Fall:
                oobima.sprite = fall;
                ads.clip = fallclip;
                break;
            case oobtype.Water:
                oobima.sprite = water;
                ads.clip = waterclip;
                break;
        }
        Menuanimation.SetTrigger("Fall");
        ads.Play();
    }

    public void respawnplayer()
    {
        p.respawn();
    }

    public void pauseplayer()
    {
        p.pause = true;
    }

    public void unpauseplayer()
    {
        p.cansetrespawn = true;
        p.pause = false;
    }
}
