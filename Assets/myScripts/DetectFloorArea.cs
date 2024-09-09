using System;
using System.Collections;
using System.Collections.Generic;
using System.IO;
using UnityEngine;
using Meta.XR.Util;
using Meta.XR.MRUtilityKit;
using TMPro;

public class DetectFloorArea : MonoBehaviour
{

    bool IsFloorDetected = false;
    [SerializeField] GameObject _debugCube;
    [SerializeField] GameObject _avatar;

    // Start is called before the first frame update
    void Start()
    {
        
    }

    // Update is called once per frame
    void Update()
    {
        if(!IsFloorDetected)
        {
            try
            {
                var surfaceType = MRUKAnchor.SceneLabels.FLOOR;
                var largestSurface = MRUK.Instance?.GetCurrentRoom()?.FindLargestSurface(surfaceType);
                if (largestSurface != null)
                {
                    Debug.Log("I find the FLOOR!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");
                    if (_debugCube != null)
                    {
                        Vector3 anchorSize = largestSurface.VolumeBounds.HasValue
                            ? largestSurface.VolumeBounds.Value.size
                            : new Vector3(0, 0, 0.01f);
                        if (largestSurface.PlaneRect.HasValue)
                        {
                            anchorSize = new Vector3(largestSurface.PlaneRect.Value.x,
                                largestSurface.PlaneRect.Value.y, 0.01f);
                        }

                        _debugCube.transform.localScale = anchorSize;
                        _debugCube.transform.localPosition = largestSurface.transform.position;
                        _debugCube.transform.localRotation = largestSurface.transform.rotation;

                        _debugCube.SetActive(true);

                        if (_avatar != null)
                        {
                            _avatar.transform.localPosition = new Vector3(largestSurface.transform.position.x, largestSurface.transform.position.y - 1.2f, largestSurface.transform.position.z);
                            _avatar.SetActive(true);
                        }

                        IsFloorDetected = true;

                        Debug.Log("I CREATE the FLOOR CUBE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!");   
                    }

                }
            }
            catch (Exception e)
            {

            }
        }
    }







}





