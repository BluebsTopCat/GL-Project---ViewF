using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.Universal;
using UnityEngine.UI;

public class PhotoCanvas : MonoBehaviour
{
    public VolumeProfile vp;
    private Bloom b;
    private ChromaticAberration ca;
    private DepthOfField dof;
    private Vignette v;
    private ColorAdjustments coa;
    public Player p;
    private RenderTexture rt;
    public RawImage i;
    public Camera c;
    public Material mr;
    public int FileCounter = 0;

    WaitForEndOfFrame frameEnd = new WaitForEndOfFrame();

    // Start is called before the first frame update
    void Start()
    {
        rt = new RenderTexture(Screen.width,Screen.height, 24, RenderTextureFormat.ARGB32, RenderTextureReadWrite.sRGB);
        i.texture = rt;
        c.targetTexture = rt;
        mr.mainTexture = rt;
        vp.TryGet(out ca);
        vp.TryGet(out b);
        vp.TryGet(out dof);
        vp.TryGet(out v);
        vp.TryGet(out coa);
    }

    public void dofon(bool b)
    {
        dof.active = b;
    }

    public void dofdist(float f)
    {
        dof.focusDistance.value = f;
    }

    public void dofstr(float st)
    {
        dof.focalLength.value = st;
    }

    public void bloomon(bool bl)
    {
        b.active = bl;
    }

    public void bloomstr(float bs)
    {
        b.intensity.value = bs;
    }

    public void bloomthreth(float bt)
    {
        b.threshold.value = bt;
    }

    public void caon(bool cao)
    {
        ca.active = cao;
    }

    public void castr(float st)
    {
        ca.intensity.value = st;
    }

    public void vignon(bool von)
    {
        v.active = von;
    }

    public void vigstr(float vstr)
    {
        v.intensity.value = vstr;
    }
    public void vigsmt(float vsmt)
    {
        v.smoothness.value = vsmt;
    }


    public void coaon(bool coo)
    {
        coa.active = coo;
    }

    public void contrast(float con)
    {
        coa.contrast.value = con;
    }

    public void saturation(float sat)
    {
        coa.saturation.value = sat;
    }

    public void exposure(float exp)
    {
        coa.postExposure.value = exp;
    }

    public void hueshift(float hsh)
    {
        coa.hueShift.value = hsh;
    }

    public void saveimage()
    {
        StartCoroutine(saveie());

    }

    IEnumerator saveie()
    {
        yield return frameEnd;
        RenderTexture.active = rt;
        Texture2D tex = new Texture2D(Screen.width, Screen.height, TextureFormat.RGBA32, false);
        tex.ReadPixels(new Rect(0, 0, rt.width, rt.height), 0, 0);
        tex.Apply();
        var Bytes = tex.EncodeToPNG();
        Destroy(tex);
        string path = Environment.GetFolderPath(Environment.SpecialFolder.Desktop) +"/";
        path = path.Replace('\\','/');
        Debug.Log(path);
        while (File.Exists(path + "Photo" + FileCounter + ".png"))
            FileCounter++;
        File.WriteAllBytes( path + "Photo" + FileCounter + ".png", Bytes);
        p.playerCurrently = Player.Playerstate.Walking;
        p.ineditor = false;
    }

    public void cancel()
    {
        p.playerCurrently = Player.Playerstate.Walking;
        p.ineditor = false;
    }

}
