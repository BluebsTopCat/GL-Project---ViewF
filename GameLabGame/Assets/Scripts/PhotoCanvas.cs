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
    public Volume vol;
    private VolumeProfile vp;
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
    public Animator caminator;
    public int FileCounter = 0;
    public Soundeffectscamera sec;

    WaitForEndOfFrame frameEnd = new WaitForEndOfFrame();

    // Start is called before the first frame update
    void Start()
    {
        rt = new RenderTexture(Screen.width,Screen.height, 24, RenderTextureFormat.ARGB32, RenderTextureReadWrite.sRGB);
        i.texture = rt;
        c.targetTexture = rt;
        mr.mainTexture = rt;
        vp = new VolumeProfile();
        vol.profile = vp;
        vp.Add<ChromaticAberration>();
        vp.Add<Bloom>();
        vp.Add<DepthOfField>();
        vp.Add<Vignette>();
        vp.Add<ColorAdjustments>();
        vp.TryGet(out ca);
        vp.TryGet(out b);
        
        vp.TryGet(out dof);
        vp.TryGet(out v);
        vp.TryGet(out coa);
    }

    public void dofon(bool b)
    {
        dof.active = b;
        dof.mode.value = DepthOfFieldMode.Bokeh;
        dof.SetAllOverridesTo(b);
        sec.playbeep();
    }

    public void dofdist(float f)
    {
        dof.focusDistance.value = f;
        sec.playbeep();
    }

    public void dofstr(float st)
    {
        dof.focalLength.value = st;
        sec.playbeep();
    }

    public void bloomon(bool bl)
    {
        b.active = bl;
        b.SetAllOverridesTo(bl);
        sec.playbeep();
    }

    public void bloomstr(float bs)
    {
        b.intensity.value = bs;
        sec.playbeep();
    }

    public void bloomthreth(float bt)
    {
        b.threshold.value = bt;
        sec.playbeep();
    }

    public void caon(bool cao)
    {
        ca.active = cao;
        ca.SetAllOverridesTo(cao);
        sec.playbeep();
    }

    public void castr(float st)
    {
        ca.intensity.value = st;
        sec.playbeep();
    }

    public void vignon(bool von)
    {
        v.active = von;
        v.SetAllOverridesTo(von);
        sec.playbeep();
    }

    public void vigstr(float vstr)
    {
        v.intensity.value = vstr;
        sec.playbeep();
    }
    public void vigsmt(float vsmt)
    {
        v.smoothness.value = vsmt;
        sec.playbeep();
    }


    public void coaon(bool coo)
    {
        coa.active = coo;
        coa.SetAllOverridesTo(coo);
        sec.playbeep();
    }

    public void contrast(float con)
    {
        coa.contrast.value = con;
        sec.playbeep();
    }

    public void saturation(float sat)
    {
        coa.saturation.value = sat;
        sec.playbeep();
    }

    public void exposure(float exp)
    {
        coa.postExposure.value = exp;
        sec.playbeep();
    }

    public void hueshift(float hsh)
    {
        coa.hueShift.value = hsh;
        sec.playbeep();
    }

    public void rotation(float rot)
    {
        c.transform.localEulerAngles = new Vector3(90,0,rot);
        sec.playbeep();
    }

    public void fov(float fo)
    {
        c.fieldOfView = fo;
        sec.playbeep();
    }

    public void saveimage()
    {
        StartCoroutine(saveie());
        
    }

    IEnumerator saveie()
    {
        sec.playbeep();
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
        caminator.SetTrigger("Print");
        cancel(false);
    }

    public void cancel(bool b)
    {
        if (b)
        {
            sec.playbeep();
            caminator.SetTrigger("Cancel");
        }
        
        c.transform.localEulerAngles = new Vector3(90,0,0);
        c.fieldOfView = 60;
        p.playerCurrently = Player.Playerstate.Walking;
        p.ineditor = false;
    }

}
