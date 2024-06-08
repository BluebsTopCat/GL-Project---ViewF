using System;
using System.Collections;
using System.Collections.Generic;
using UnityEditor;
using UnityEngine;
using Random = UnityEngine.Random;

public class PlantArea : MonoBehaviour
{
    public Biome b;

    public float radius;

    public float density;
    

    public LayerMask validplacement;

    private List<GameObject> plants;

    [InspectorButton("Generate")] public bool generate;

    // Start is called before the first frame update
    private void Generate()
    {
        generate = false;
        if(plants != null)
            foreach (GameObject g in plants)
            {
                DestroyImmediate(g);
            }

        plants = new List<GameObject>();

        int numberOfPlants = Mathf.RoundToInt(density / 10 * (Mathf.PI * radius * radius));
        for (int i = 0; i < numberOfPlants; i++)
        {
            PlantWeightPair p = b.Plants[getRandomWeighted(b)];
            GameObject output = PrefabUtility.InstantiatePrefab(p.getMesh(), this.transform) as GameObject;
            output.transform.position = getValidPos();
            output.transform.rotation = p.getRot();
            output.transform.localScale = Vector3.one * p.getSize();
            plants.Add(output);
        }
    }

    private int getRandomWeighted(Biome biome)
    {
        List<int> outputs = new List<int>();

        int index = 0;
        foreach (PlantWeightPair p in biome.Plants)
        {
            for (int i = 0; i < p.Weight; ++i)
            {
                outputs.Add(index);
            }

            index += 1;
        }

        return (outputs[Random.Range(0, outputs.Count)]);
    }

    private void OnDrawGizmos()
    {
        Gizmos.DrawWireSphere(this.transform.position, radius);
    }

    private Vector3 getValidPos()
    {

        int errorcatch = 0;
        while (true)
        {
            Vector2 circlepoint = Random.insideUnitCircle * radius;
            Vector3 globalizedUp = this.transform.position + new Vector3(circlepoint.x, radius, circlepoint.y);

            RaycastHit h;
            Ray r = new Ray(globalizedUp, Vector3.down);
            if (Physics.Raycast(r, out h, radius * 2, validplacement))
            {
                return h.point;
            }

            errorcatch += 1;
            if (errorcatch > 10)
            {
                throw (new Exception("No valid positions found"));

            }
        }

        return Vector3.zero;
    }
}
