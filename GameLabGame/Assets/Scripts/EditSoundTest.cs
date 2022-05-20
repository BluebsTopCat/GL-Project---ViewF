using System;
using System.Collections.Generic;
using UnityEngine;
 
#if UNITY_EDITOR
using UnityEditor;
/// <summary>
/// 
/// </summary>
[ExecuteInEditMode]
public class EditSoundTest : MonoBehaviour
{
    public bool definingregions;
    public int currentregion = 0;
    public List<region> regions;
    // Start is called before the first frame update
    private void OnEnable()
    {
        if (!Application.isEditor)
        {
            Destroy(this);
            
        }
        SceneView.onSceneGUIDelegate += OnScene;
    }

    void OnScene(SceneView scene)
    {
        Event e = Event.current;

        if (e.type == EventType.MouseDown && e.button == 0)
        {
            if (definingregions == false)
            {
                currentregion = regions.Count;
                regions.Add(new region(null));
                definingregions = true;
            }
            Vector3 mousePos = e.mousePosition;
            float ppp = EditorGUIUtility.pixelsPerPoint;
            mousePos.y = scene.camera.pixelHeight - mousePos.y * ppp;
            mousePos.x *= ppp;
            Vector3 point = scene.camera.ScreenToWorldPoint(mousePos);
            regions[currentregion].points.Add(point);
            refreshregions();
        }
        else if (e.type == EventType.MouseDown && e.button == 1)
        {
            definingregions = false; 
        }
    }

    private void OnDrawGizmos()
    {
        foreach (region r in regions)
        {
            Gizmos.color = r.c;
            foreach (Vector3 p in r.points)
            {
                Gizmos.DrawWireSphere(p, .5f);
            }
            Gizmos.DrawWireMesh(r.m);   
        }
    }

    private void refreshregions()
    {
        foreach (region r in regions)
        {
            Mesh m = new Mesh();
            m.vertices = new Vector3[r.points.Count];
            m.triangles = new int[r.points.Count];
            for(int i = 0; i < r.points.Count; i++)
            {
                m.vertices[i] = r.points[i];
                m.triangles[i] = i;
            }
            r.m = m;
        }
    }
}
#endif

[System.Serializable]
public class region
{
    public List<Vector3> points;
    [HideInInspector]
    public Mesh m;
    public Color c;
    public region(List<Vector3> p)
    {
        points = p;
    }
}