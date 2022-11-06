package com.example.myapp.fragmentsSport;

import android.content.Intent;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ExpandableListView;
import android.widget.Toast;

import com.example.myapp.R;
import com.example.myapp.fragmentsSport.expandableListSport.SportExpandableListData;
import com.example.myapp.fragmentsSport.expandableListSport.SportExpandableListAdapter;
import com.example.myapp.fragmentsSport.expandableListSport.SportExpandableListItem;
import com.example.myapp.subActivities.DataSleep;
import com.example.myapp.subActivities.DataSport;
import com.google.android.material.floatingactionbutton.FloatingActionButton;

import java.util.ArrayList;
import java.util.List;

/**
 * A simple {@link Fragment} subclass.
 * Use the {@link SportList#newInstance} factory method to
 * create an instance of this fragment.
 */
public class SportList extends Fragment {

    // TODO: Rename parameter arguments, choose names that match
    // the fragment initialization parameters, e.g. ARG_ITEM_NUMBER
    private static final String ARG_PARAM1 = "param1";
    private static final String ARG_PARAM2 = "param2";

    // TODO: Rename and change types of parameters
    private String mParam1;
    private String mParam2;

    public SportList() {
        // Required empty public constructor
    }

    /**
     * Use this factory method to create a new instance of
     * this fragment using the provided parameters.
     *
     * @param param1 Parameter 1.
     * @param param2 Parameter 2.
     * @return A new instance of fragment SportList.
     */
    // TODO: Rename and change types and number of parameters
    public static SportList newInstance(String param1, String param2) {
        SportList fragment = new SportList();
        Bundle args = new Bundle();
        args.putString(ARG_PARAM1, param1);
        args.putString(ARG_PARAM2, param2);
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() != null) {
            mParam1 = getArguments().getString(ARG_PARAM1);
            mParam2 = getArguments().getString(ARG_PARAM2);
        }
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
                             Bundle savedInstanceState) {
        // Inflate the layout for this fragment
        return inflater.inflate(R.layout.fragment_sport_list, container, false);
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        List<SportExpandableListItem> sportExpandableListItemList = new ArrayList<>();

        List<SportExpandableListData> sportExpandableListDataList = new ArrayList<>();
        List<SportExpandableListData> sportExpandableListDataList1 = new ArrayList<>();

        sportExpandableListDataList.add(new SportExpandableListData("lol", 0, 0));
        sportExpandableListDataList.add(new SportExpandableListData("lol", 0, 0));

        sportExpandableListDataList1.add(new SportExpandableListData("lol1", 0, 0));
        sportExpandableListDataList1.add(new SportExpandableListData("lol1", 0, 0));

        sportExpandableListItemList.add(new SportExpandableListItem("test", sportExpandableListDataList));
        sportExpandableListItemList.add(new SportExpandableListItem("test1", sportExpandableListDataList1));

        ExpandableListView expandableListView = requireView().findViewById(R.id.sportExpandableListView);
        SportExpandableListAdapter sportExpandableListAdapter = new SportExpandableListAdapter(getContext(), sportExpandableListItemList);
        expandableListView.setAdapter(sportExpandableListAdapter);

        expandableListView.setOnGroupExpandListener(new ExpandableListView.OnGroupExpandListener() {
            int lastExpandedPosition = -1;
            @Override
            public void onGroupExpand(int i) {
                if(lastExpandedPosition != -1 && i != lastExpandedPosition){
                    expandableListView.collapseGroup(lastExpandedPosition);
                }
                lastExpandedPosition = i;
            }
        });

        expandableListView.setOnChildClickListener((expandableListView1, view1, i, i1, l) -> {
            String selected = sportExpandableListAdapter.getChild(i, i1).toString();
            Toast.makeText(getContext(), selected, Toast.LENGTH_SHORT).show();
            return true;
        });

        FloatingActionButton floatingActionButton = requireView().findViewById(R.id.buttonFloating);
        floatingActionButton.setOnClickListener(view1 -> {
            startActivity(new Intent(getContext(), DataSport.class));
            getActivity().overridePendingTransition(0, 0);
        });
    }
}